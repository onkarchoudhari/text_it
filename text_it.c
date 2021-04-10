/*** includes ***/

/* feature test macro: 
 * https://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html */
/* getline() won't throw an error after this */
#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>


/*** defines ***/

#define TEXT_IT_VERSION     "0.0.1"
#define TEXT_IT_TAB_STOP    8
#define TEXT_IT_QUIT_TIMES  3   /* If file modified, user needs to press Ctrl-Q 3 more times in order to quit without saving */

#define CTRL_KEY(k)         ((k) & 0x1f)  
/* clears upper 3 bits, equivalent to what ctrl does (strips bit 5 and 6 (0 to 7)) */

enum editorKey {
    BACKSPACE = 127,    /* doesn't have a human-readable representation */
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY, /* NOTE: Depending on OS or terminal escape seq for HOME and END can be different. Covered all the cases */
    END_KEY,
    PAGE_UP, 
    PAGE_DOWN
};

enum editorHighlight {  /* contains possible values that `hl` array can contain */
    HL_NORMAL = 0,
    HL_COMMENT, /* Highlighting single-line comments */
    HL_MLCOMMENT,   /* Highlighting multi-line comments */
    /* two types of keywords, highlighted in different colors */
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,  /* Highlighting strings */
    HL_NUMBER,  /* Highlighting numbers */  
                /* every char that's part of a number will have HL_NUMBER in `hl` array */
    HL_MATCH    /* highlighting search results */ 
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** data ***/

/* contains all the syntax highlighting info for a particular filetype */
struct editorSyntax {
    char *filetype;     /* name of filetype, displayed in status bar */

    char **filematch;   
    /* array of strings - each string contains a pattern to match a filename against */
    /* if filename matches, file will be recognized as having that filetype */
    
    char **keywords;    /* NULL terminated array of strings */
    /* second type of keywords are | (pipe) terminated */

    char *singleline_comment_start; /* each language specify its own single-line comment pattern */
    /* set to NULL or empty string if not in use */

    char *multiline_comment_start;
    char *multiline_comment_end;

    int flags;  
    /* flags for whether to highlight numbers and whether to highlight strings for that filetype */
};

/* to store row of text in the editor */
typedef struct erow {
    int idx;    /* index of the row within the file */ 
                /* allows each row to know previous rows `hl_open_comment` val */
    int size;
    int rsize;
    char *chars;    /* stores line of text as a pointer to dynamically-allocated character data */
    char *render;   /* contains the actual characters to draw on the screen for the given row of text */
                    /* for tabs and non-printable control characters e.g. ^A for ctrl-A */
    unsigned char *hl;  /* stores the highlighting of each line in an array */
                        /* each value in the array corresponds to a character in `render` and will tell whether
                         * char is part of a string, or a comment, or a number, and so on */
    int hl_open_comment; /* bool - tells if line is part of an unclosed multi-line comment */
} erow;

struct editorConfig {
    int cx, cy;            /* cursor's x and y position */
                           /* cx  - index into the `char` field of an erow */
    int rx;                /* index into the `render` field. If no tabs on the current line, rx = cx */
    int rowoff;            /* keeps track of what row of the file the user is currently scrolled to */
                           /* as per understanding -
                            * how much the user has moved from line 0 (if pressing ARROWDOWN continuously)
                            * how much lines are to be shown after scrolling from default screen */
    int coloff;            /* used as an index into the char of each erow */
    int screenrows;
    int screencols;
    int numrows;
    erow *row;             /* stores multiple lines. Dynamically allocated array */
    int dirty;             /* to keep track of whether the text loaded in editor differs from what's in file */
    char *filename;
    char statusmsg[80];    /* status message below status bar */
    time_t statusmsg_time; /* timestamp for the msg, so we can erase msg after few secs */
    struct editorSyntax *syntax;
    struct termios orig_termios;    /* to save terminal's original state */
};

struct editorConfig E;


/*** filetypes ***/

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL }; /* Array must be NULL terminated */
char *C_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else", 
    "struct", "union", "typedef", "static", "enum", "class", "case", 

    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|", 
    "void|", NULL
};

struct editorSyntax HLDB[] = {      /* HLDB - "highlight database" */
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
};

/* to store the length of the HLDB array */
#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))   


/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));


/*** terminal ***/

/* ERROR HANDLING */
/* prints error message and exits the program */
void die(const char *s) {
    /* If an error occurs in the middle of rendering the screen, we don’t want a bunch of garbage left over on the screen, 
     * and we don’t want the error to be printed wherever the cursor happens to be at that point
     * clear the screen and position the cursor before exiting */
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    perror(s);
    exit(1);
}

/* when exiting, restores the terminal's original state */
void disableRawMode() {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
        die("tcsetattr");
}

/* to enable the raw mode */
void enableRawMode() {
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)   die("tcgetattr");
    atexit(disableRawMode);

    struct termios raw = E.orig_termios;
    /* BRKINT   if ON - break condition will cause a SIGINT signal to be sent to the program (like pressing ctrl-C)
     * ICRNL    to fix ctrl-M. Terminal translates \r to \n, turns off this feature (ICRNL: Input flag Carriage Return New Line) 
     * INPCK    parity checking (not applicable to modern terminal emulators)
     * ISTRIP   if ON - cause 8th bit of each input byte to be stripped, meaning it will set it to 0. 
     * IXON     to turn off ctrl-S and ctrl-Q 
     */ 
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);

    /* OPOST    to turn off output processing. \n is translated to \r\n, turning off this feature. */
    raw.c_oflag &= ~(OPOST);
    
    /* CS8      sets the character size to 8 bits per byte */
    raw.c_cflag |= (CS8);

    /* ECHO     to turn off echoing
     * ICANON   to turn off canonical mode
     * IEXTEN   to disable ctrl-V
     * ISIG     to turn off ctrl-C and ctrl-Z
     */
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

    /* TIMEOUT FOR READ
     * c_cc     control characters: array of bytes that control various terminal settings
     * VMIN     sets minimum number of bytes of input needed before read() can return
     *          We set it to 0 so that read() returns as soon as there is any input to be read
     * VTIME    maximum amount of time to wait before read() returns (set to 1/10 second)
     */
    raw.c_cc[VMIN]  = 0;
    raw.c_cc[VTIME] = 1;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)  die("tcsetattr");
}

/* waits for 1 key press and returns it */
/* deals with low-level terminal input */
int editorReadKey() {
    int nread;
    char c;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN)  die("read");   /* In Cygwin, when read times out it return -1 with EAGAIN, instead of 0
                                                             * Don't treat EAGAIN as error */
    }

    if (c == '\x1b') {
        char seq[3];

        if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
                if (seq[2] == '~') {
                    switch (seq[1]) {
                        case '1': return HOME_KEY;
                        case '3': return DEL_KEY;
                        case '4': return END_KEY;
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        case '7': return HOME_KEY;
                        case '8': return END_KEY;
                    }
                }
            } else {
                switch (seq[1]) {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }

        return '\x1b';
    } else {
        return c;
    }
}

int getCursorPosition(int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;

    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4)    return -1;

    while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1)     break;
        if (buf[i] == 'R')      break;
        i++;
    }
    buf[i] = '\0';

    if (buf[0] != '\x1b' || buf[1] != '[')  return -1;
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2)  return -1;

    return 0;
}

int getWindowSize(int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {   
        /* TIOCGWINSZ: Terminal Input Output Control Get WINdow SiZe */
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12)   return -1;  /* C: cursor forward; B: cursor down */
        /* ioctl() isn’t guaranteed to be able to request the window size on all systems,
         * so we are going to provide a fallback method of getting the window size.
         * The strategy is to position the cursor at the bottom-right of the screen,
         * then use escape sequences that let us query the position of the cursor.
         * That tells us how many rows and columns there must be on the screen. */
        return getCursorPosition(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}


/*** syntax highlighting ***/

/* takes a character and returns true if it's considered a separator character */
int is_separator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

/* traverse through the characters of an `erow` and highlight them by setting each value in the `hl` array */
void editorUpdateSyntax(erow *row) {
    row->hl = realloc(row->hl, row->rsize); /* new row or row size can increase from the last time */
    memset(row->hl, HL_NORMAL, row->rsize);

    if (E.syntax == NULL) return;

    char **keywords = E.syntax->keywords;

    char *scs = E.syntax->singleline_comment_start;
    char *mcs = E.syntax->multiline_comment_start;
    char *mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;

    int prev_sep = 1;   /* begin of the line is a separator */
    int in_string = 0;  /* keeps track of whether currently inside a string */
    int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row->size) {     /* while loop will allow us to consume multiple characters each iteration */
        char c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        if (scs_len && !in_string && !in_comment) {    
            /* check if not inside string, and single line comment shouldn't be detected in multiline comment */
            if (!strncmp(&row->render[i], scs, scs_len)) {  /* check if char is start of single-line comment */
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);    /* set the whole rest of the line */
                break;
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    /* we are at the end of the multiline comment */
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;    /* Not at the end - only consume char; already highlighted */
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                /* we are at the begining of the multiline comment */
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->rsize) {  /* if in string and current char is a backslash */
                    row->hl[i + 1] = HL_STRING;
                    i += 2;     /* consume both the characters */
                    continue;
                }
                if (c == in_string) in_string = 0;
                i++;
                prev_sep = 1;   /* closing quote considered as separator */
                continue;
            } else {
                if (c == '"' || c == '\'') {    /* double and single quoted strings */
                    in_string = c;  /* store the character - to know closing quotes */
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }


        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) || 
                (c == '.' && prev_hl == HL_NUMBER)) {   /* Decimal point */
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = 0;
                continue;
            }
        }
        
        /* keywords require a separator both before and after the keyword */
        if (prev_sep) { /* separator before keyword, before looping through each possible keyword */
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2  = keywords[j][klen - 1] == '|';
                if (kw2) klen--;

                if (!strncmp(&row->render[i], keywords[j], klen) &&
                    is_separator(row->render[i + klen])) {  /* AND we check if separator char comes after keyword */
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;  /* consume entire keyword */
                    break;      /* break from `for` loop */
                }
            }
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue;
            }
        }

        prev_sep = is_separator(c);
        i++;
    }

    int changed = (row->hl_open_comment != in_comment);
    /* set value of current row's `hl_open_comment` to whatever state 
     * `in_comment` is left in after processing entire row */
    row->hl_open_comment = in_comment;

    /* Updates syntax of the next lines in the file */
    /* however, highlight of next line won't change if the value of this line's `hl_open_comment` did not change */
    if (changed && row->idx + 1 < E.numrows) {
        editorUpdateSyntax(&E.row[row->idx + 1]);
        /* continue to propogate to more and more lines until one line is unchanged
         * at this point all the lines after that must be unchanged */
    }
}

/* maps values in `hl` to the actual ANSI color codes */
int editorSyntaxToColor(int hl) {
    switch(hl) {
        case HL_COMMENT:
        case HL_MLCOMMENT:  return 36;  /* foreground cyan */
        case HL_KEYWORD1:   return 33;  /* foreground yellow */
        case HL_KEYWORD2:   return 32;  /* foreground green */
        case HL_STRING:     return 35;  /* foreground magenta */
        case HL_NUMBER:     return 31;  /* foreground red */
        case HL_MATCH:      return 34;  /* foreground blue */
        default:            return 37;  /* foreground white */
    }
}

/* matches the current filename to one the filematch fields in HLDB */
void editorSelectSyntaxHighlight() {
    E.syntax = NULL;
    if (E.filename == NULL) return;

    char *ext = strrchr(E.filename, '.');   
    /* returns a ptr to the last occurrence of a char (here '.') */
    /* if no extension then ext = NULL */
    
    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;
        while (s->filematch[i]) {
            int is_ext = (s->filematch[i][0] == '.');
            if ((is_ext && ext && !strcmp(ext, s->filematch[i])) || 
                (!is_ext && strstr(E.filename, s->filematch[i]))) { /* if not extension pattern, 
                                                                     * then we check to see if the pattern 
                                                                     * exists anywhere in the filename */
                E.syntax = s;   /* if filename matched, we set E.syntax to current struct */

                int filerow;
                for (filerow = 0; filerow < E.numrows; filerow++) {
                    editorUpdateSyntax(&E.row[filerow]);
                }
            
                return;
            }
            i++;
        }
    }
}

/*** row operations ***/

/* converts a `chars` index to a `render` index */
int editorRowCxToRx(erow *row, int cx) {
    int rx = 0;
    int j;
    for (j = 0; j < cx; j++) {
        if (row->chars[j] == '\t') 
            rx += (TEXT_IT_TAB_STOP - 1) - (rx % TEXT_IT_TAB_STOP);
            /* we use (rx % TAB_STOP) to find out how many columns we are to the right of the last tab stop,
             * and subtract that to find out how many we are to the left of the next tab stop. 
             * We reach just to the left of next tab stop  */
        rx++; 	/* unconditional add, we place us to next tab stop */
    }
    return rx;
}

/* converts a `row` index to a `chars` index */
int editorRowRxToCx(erow *row, int rx) {
    int cur_rx = 0;
    int cx;
    for (cx = 0; cx < row->size; cx++) {
        if (row->chars[cx] == '\t')
            cur_rx += (TEXT_IT_TAB_STOP - 1) - (cur_rx % TEXT_IT_TAB_STOP);
	cur_rx++;

        if (cur_rx > rx) return cx;
    }

    return cx;  /* In case caller provides an `rx` out of range */
}

/* uses the chars string of an erow to fill in the contents of the render string */
void editorUpdateRow(erow *row) {
    int tabs = 0;
    int j;
    for (j = 0; j < row->size; j++)
        if (row->chars[j] == '\t') tabs++;

    free(row->render);
    /* each tab requires max 8 characters */
    /* row->size already counts 1 for each tab, so we multiply by 7 */
    row->render = malloc(row->size + tabs*(TEXT_IT_TAB_STOP - 1) + 1);

    int idx = 0;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
        row->render[idx++] = ' '; /* each tab will give at least 1 space */
        /* append spaces until we get to a tab stop, which is a column that is divisible by 8 */
        /* we can relate - if we press tab on second line, it alligns with the first line */
        while (idx % TEXT_IT_TAB_STOP != 0) row->render[idx++] = ' '; 
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
    row->render[idx] = '\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
    if (at < 0 || at > E.numrows) return;

    E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));

    /* update idx of each row whenever a row is inserted into the file */
    for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;

    E.row[at].idx = at; /* row's index at the time it is inserted */

    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].rsize = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hl_open_comment = 0;
    editorUpdateRow(&E.row[at]);

    E.numrows++;
    E.dirty++;
}

/* frees the memory owned by the `erow` which is deleted */
void editorFreeRow(erow *row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}

/* Deletes a row (when we press backspace at the start of line) */
void editorDelRow(int at) {
    if (at < 0 || at >= E.numrows) return;
    editorFreeRow(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));

    /* update idx of each row whenever a row is deleted from the file */
    for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
    E.numrows--;
    E.dirty++;
}

/* Insert ordinary characters */
/* Does not have to worry about the cursor position */
void editorRowInsertChar(erow *row, int at, int c) {
    if (at < 0 || at > row->size) at = row->size;
    row->chars = realloc(row->chars, row->size + 2); /* 2 -> one for char and other for null byte */
    /* (TODO: Didn't get this - '\0' was already added to the string when we call editorAppendRow()
     * NOTE: `editorAppendRow()` is called only 1st time while displaying the rows in `editorOpen()` 
     * (next time only if new line to be added to file in `editorInsertChar()`) 
     */

    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1); 
    /* similar to memcpy, but safe to use when src and dest arrays overlap */

    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty++;
}

/* Appends string to the end of a row (when we use backspace at the start of line) */
void editorRowAppendString(erow *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1); /* +1 for null byte */
    memcpy(&row->chars[row->size], s, len);
    row->size += len;       /* NOTE - '\0' is not included in row->size */
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty++;
}

/* deletes a character in an `erow` */
void editorRowDelChar(erow *row, int at) {
    if (at < 0 || at >= row->size) return;
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    editorUpdateRow(row);
    E.dirty++;
}

/*** editor operations ***/

/* Does not worry about the details of modifying an erow */
void editorInsertChar(int c) {
    if (E.cy == E.numrows) {    /* Implies cursor on tilde line */
        editorInsertRow(E.numrows, "", 0); /* append a new row to the file before inserting character. Memory allocation takes place here only  */
    }
    editorRowInsertChar(&E.row[E.cy], E.cx, c); /* Insert char at the cursor location */
    E.cx++; /* so the next character the user inserts will go after the character just inserted */
}

/* handles an ENTER keypress */
void editorInsertNewline() {
    if (E.cx == 0) {    /* Insert a new blank row before the line we are on */
        editorInsertRow(E.cy, "", 0);
    } else {            /* Split the line */
        erow *row = &E.row[E.cy];
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy]; /* we reassign because `realloc()` in `editorInsertRow()` might move memory around on us and invalidate the pointer */
        row->size = E.cx;
        row->chars[row->size] = '\0';
        editorUpdateRow(row);   /* Call this for previous row, `editorInsertRow()` already calls this function for the new row */
    }
    E.cy++;
    E.cx = 0;
}

/* deletes the character that is to the left of the cursor */
void editorDelChar() {
    if (E.cy == E.numrows) return;
    if (E.cx == 0 && E.cy == 0) return;

    erow *row = &E.row[E.cy];

    if (E.cx > 0) {
        editorRowDelChar(row, E.cx - 1);
        E.cx--;
    } else {    /* backspace at the start of the line */
        E.cx = E.row[E.cy - 1].size;
        editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
        editorDelRow(E.cy);
        E.cy--;
    }
}

/*** file i/o ***/

/* converts array of erow structs into a single string */
char *editorRowsToString(int *buflen) {
    int totlen = 0;
    int j;
    for (j = 0; j < E.numrows; j++) {
        totlen += E.row[j].size + 1;    /* added len of each line. +1 for \n at the end */
    }
    *buflen = totlen;

    char *buf = malloc(totlen);
    char *p = buf;
    for (j = 0; j < E.numrows; j++) {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }
    
    return buf;     /* Expecting the caller to `free()` the memory */
}

void editorOpen(char *filename) {
    free(E.filename);
    /* store filename to display it in status bar */
    E.filename = strdup(filename);  /* strdup - makes a copy of the given string, allocating the required memory, 
                                     * "assuming you will free that memory" */

    editorSelectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");
    if (!fp) die("fopen");

    char *line = NULL;
    size_t linecap = 0; 
    ssize_t linelen;
    /* 
     * getline [manual entry] - 
     * If *lineptr is set to NULL and *n is set 0 before the call, then getline() will allocate a buffer for storing the line.  
     * This buffer should be freed by the user program even if getline() failed.
     * This will also set linecap value.
     * Alternatively,  before  calling getline(), *lineptr can contain a pointer to a malloc(3)-allocated buffer *n bytes in size. 
     * If the buffer is not large enough to hold the line, getline() resizes  it with realloc(3), updating *lineptr and *n as necessary
     *
     * linecap = line capacity 
     * returns   len of line it reads
     */
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        while (linelen > 0 && (line[linelen - 1] == '\n' ||
                               line[linelen - 1] == '\r'))      /* strip off \n and \r as each line would be new line */
            linelen--;
        editorInsertRow(E.numrows, line, linelen);
    }

    free(line);
    fclose(fp);
    E.dirty = 0;
}

/* Writes the string returned by `editorRowsToString()` to disk */
void editorSave() {
    if (E.filename == NULL) {
        E.filename = editorPrompt("Save as: %s", NULL);
        if (E.filename == NULL) {
            editorSetStatusMessage("Save aborted");
            return;
        }
        editorSelectSyntaxHighlight();
    }

    int len;
    char *buf = editorRowsToString(&len);

    /* Flags - read and write; create new file if it doesn't already exist 
     * O_CREAT requires 1 more argument - mode (permissions). 0644 = std read-write permission */
    int fd = open(E.filename, O_RDWR | O_CREAT, 0644); 
    if (fd != -1) {
        if (ftruncate(fd, len) != -1) {
            /* sets the file's size */  /* if file larger than len - truncate data; if shorter - add '0' bytes at the end to make file of that len */
            if (write(fd, buf, len) == len) {
                close(fd);
                free(buf);
                E.dirty = 0;
                editorSetStatusMessage("%d bytes written to disk", len);
                return;
            }
        }
        close(fd);
    }

    free(buf);
    editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));

    /* TIP - normal way to overwrite a file is to pass `O_TRUNC` flag to `open()` - truncates the file completely, making it an empty file, before writing new data. 
     * By truncating the file ourselves to the same length as the data we are planning to write into it, 
     * we are making the whole overwriting operation a little bit safer in case the ftruncate() call succeeds but the write() call fails.
     * In that case, the file would still contain most of the data it had before.
     * But if the file was truncated completely by the open() call and then the write() failed, you’d end up with all of your data lost
     *
     * More advanced editors will write to a new, temporary file, and then rename that file to the actual file the user wants to overwrite, 
     * and they’ll carefully check for errors through the whole process. */

}


/*** find ***/

/* for 'Incremental Search' - file is searched after each keypress when user is typing search query */
/* callback function - called after each keypress */
/* Loops through all the rows of the file and if a row contains query string, moves the cursor to the match */
void editorFindCallback(char *query, int key) {
    static int last_match = -1; /* index of the row that the last match was on, or -1 if no last match */
    static int direction = 1;   /* 1: searching forward; -1: searching backward */

    static int saved_hl_line;
    static char *saved_hl = NULL;

    if (saved_hl) {
        memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
        free(saved_hl);
        saved_hl = NULL;
    }

    if (key == '\r' || key == '\x1b') {
        last_match = -1;    /* Reset to initial values */
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {   /* Forward search */
        direction = 1;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {      /* Backward search */
        direction = -1;
    } else {
        last_match = -1;
        direction = 1;
    }

    /* starts at the top of the file */
    if (last_match == -1) direction = 1;
    int current = last_match;   /* index of the current row we are searching */
    int i;
    for (i = 0; i < E.numrows; i++) {
        current += direction;
        /* Wrapping around search */
        if (current == -1) current = E.numrows - 1;
        /* last time reached last line and doing backward search, so direction = -1. Thus current = 0 - 1 = -1 */
        else if (current == E.numrows) current = 0;
        /* last time reached last line and doing forward search. Thus current = last + 1 = E.numrows */

        erow *row = &E.row[current];
        char *match = strstr(row->render, query);
        /* strstr - checks if query is a substring of row->render;
         * returns NULL if no match; returns a pointer to the matching substring */
        if (match) {
            last_match = current;   /* so the next search start from the last search */
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row->render); /* subtract pointers pointing in same string */
            E.rowoff = E.numrows;   /* matching line will be at very top of the screen */
            
            saved_hl_line = current;
            saved_hl = malloc(row->rsize);
            memcpy(saved_hl, row->hl, row->rsize);
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

void editorFind() {
    /* Saving the cursor position and scroll position
     * to restore them when pressed ESC while searching */
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)", editorFindCallback);

    if (query) {
        free(query);
    } else {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}

/*** append buffer ***/

/* Instead of making small writes every time we refresh the screen, we make 1 big write which makes sure that 
 * the whole screen is updated at once. This would remove flicker effect when there are small pauses between writes */
struct abuf {
    char *b;
    int len;
};

/* Empty buffer */
#define ABUF_INIT {NULL, 0}         /* Acts as a constructor for our abuf type */

/* Appends a write to the append buffer */
void abAppend(struct abuf *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len);  /* realloc retains the previous data */

    if (new == NULL)    return;
    memcpy(&new[ab->len], s, len);      /* appends the new write */
    ab->b = new;
    ab->len += len;
}

/* Append Buffer Destructor */
void abFree(struct abuf *ab) {
    free(ab->b);
}

/*** output ***/

/* check if the cursor has moved outside of the visible window, and if so, 
 * adjust E.rowoff so that the cursor is just inside the visible window */
/* E.rowoff refers to what's at the top of the screen, and we wish to talk about what's at the bottom */
void editorScroll() {
   /* scrolling should take into account the characters that are 
    * actually rendered to the screen, and the rendered position of the cursor */
    E.rx = 0;
    if (E.cy < E.numrows) {
        E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
    }

    if (E.cy < E.rowoff) {
        /* check if cursor above visible window, if yes, scroll up to where the cursor is */
        E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
        /* if past the bottom of the visible window */
        E.rowoff = E.cy - E.screenrows + 1;
    }
    if (E.rx < E.coloff) {
        E.coloff = E.rx;
    }
    if (E.rx >= E.coloff + E.screencols) {
        E.coloff = E.rx - E.screencols + 1;
    }
}

void editorDrawRows(struct abuf *ab) {
    int y;
    for (y = 0; y < E.screenrows; y++) {
        int filerow = y + E.rowoff;     /* used as an index into E.row */
        if (filerow >= E.numrows) {
            if (E.numrows == 0 && y == E.screenrows/3) {    /* print welcome message at 1/3rd screen */
                char welcome[80];
                int welcomelen = snprintf(welcome, sizeof(welcome), "Text_it editior -- version %s", TEXT_IT_VERSION);
                if (welcomelen > E.screencols)  welcomelen = E.screencols;

                /* center allign the text */
                int padding = (E.screencols - welcomelen) / 2; /* divide screen in 2 and then subtract half string len */
                if (padding) {
                    abAppend(ab, "~", 1);   /* first char should be '~' */
                    padding--;
                }
                while (padding--)   abAppend(ab, " ", 1);   /* put spaces till you reach the expected position */
                
                abAppend(ab, welcome, welcomelen);
            } else {
                /* draws a column of tildes on the left side */
                abAppend(ab, "~", 1);
            }
        } else {
            int len = E.row[filerow].rsize - E.coloff;
            if (len < 0) len = 0;
            if (len > E.screencols) len = E.screencols;
            char *c = &E.row[filerow].render[E.coloff];
            unsigned char *hl = &E.row[filerow].hl[E.coloff];
            int current_color = -1; /* -1: default text color */
            int j;
            for (j = 0; j < len; j++) {
                if (iscntrl(c[j])) {
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    /* Capital letters come after '@' */
                    /* '?' if not in the alphabetic range */
                    abAppend(ab, "\x1b[7m", 4); /* invert colors */
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);  /* turn off invert */
                    /* this turns off ALL text formatting, including colors. 
                     * So we reprint the escape sequence for the current color */
                    if (current_color != -1) { 
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, clen);
                    }
                } else if (hl[j] == HL_NORMAL) {
                    if (current_color != -1) {
                        /* when color changes we print escape seq for new color */
                        abAppend(ab, "\x1b[39m", 5); 
                        current_color = -1;
                    }
                    abAppend(ab, &c[j], 1);
                } else {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != current_color) {
                        current_color = color;  /* set current_color to new color */
                        char buf[16];
                        /* when color changes we print escape seq for new color */
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);    /* reset text color to default */
        }

        abAppend(ab, "\x1b[K", 3);      /* Instead of clearing entire screen before each refresh, 
                                         * it seems more optimal to clear each line as we redraw them */
        abAppend(ab, "\r\n", 2);
    }
}

void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[7m", 4);     /* Switches to inverted colors */ 
    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s", 
                        E.filename ? E.filename : "[NO Name]", E.numrows, E.dirty ? "(modified)" : "");  
    /* %.20s - display up to 20 characters */
    
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d", 
            E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);

    if (len > E.screencols) len = E.screencols;
    abAppend(ab, status, len);

    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        } else {
            abAppend(ab, " ", 1);
            len++;
        }
    }
    abAppend(ab, "\x1b[m", 3);      /* Switches back to normal formatting */
                                    /* argument of 0 clears all attributes */
    abAppend(ab, "\r\n", 2);        /* Newline for message bar */
}

/* To draw Message bar */
void editorDrawMessageBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3); /* clear msg bar */
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) msglen = E.screencols;
    if (msglen && time(NULL) - E.statusmsg_time < 5) /* display the msg only if the msg is less than 5 seconds old */
        abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen() {
    editorScroll();

    struct abuf ab = ABUF_INIT;

    /* \x1b: escape character (1b in hex) followed by '['; J: cmd to clear screen; 2: argument to the cmd */
    abAppend(&ab, "\x1b[?25l", 6);  /* hides the cursor */
    abAppend(&ab, "\x1b[H", 3);     
    /* position cursor (no arguments => 1,1 (numbering starting from 1 and not 0) by default) */

    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);

    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1, (E.rx - E.coloff) + 1);
    abAppend(&ab, buf, strlen(buf));  

    abAppend(&ab, "\x1b[?25h", 6);  /* shows the cursor */

    write(STDOUT_FILENO, ab.b, ab.len);  
    abFree(&ab);
}

/* variadic function -> can take any no. of arguments */
void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;     /* C's way of dealing with variadic functions */
    va_start(ap, fmt); 
    /* past argument before `...` is passed so that the address of the next arguments is known */
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    /* takes care of reading the format string and calling `va_arg()` to get each argument */
    va_end(ap);
    E.statusmsg_time = time(NULL);  /* returns no. of secs passed since midnight, 01-01-1970 */
}

/*** input ***/

/* displays a prompt in the status bar, and lets the user input a line of text after the prompt (used for saving new file) */
/* prompt is expected to be a format string containing a `%s`, which is where the user's input will be displayed */
char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = malloc(bufsize);    /* stores user's input */

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {     /* Repeatedly sets the status message, refreshes the screen, and waits for a keypress to handle */
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {  
            /* All 3 does the work of backspace (for last character only, no cursor movement */
            if (buflen != 0) buf[--buflen] = '\0';
        } else if (c == '\x1b') {
            editorSetStatusMessage("");
	    if (callback) callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') {                 /* pressed enter */
            if (buflen != 0) {
                editorSetStatusMessage("");     /* clear the status message */
		if (callback) callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {    /* Append printable character to buf */ 
                                                /* Made sure input key isn't one of the special keys in the `editorKey` enum */
            if (buflen == bufsize - 1) {       /* if max capacity reached, we double buf size */ 
                bufsize *= 2;
                buf = realloc(buf, bufsize);
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';     /* `editorSetStatusMessage()` and caller of `editorPrompt()` will use it to know end of string */
        }

	if(callback) callback(buf, c);
    }
}

void editorMoveCursor(int key) {
    /* Limits y movement, allows only 1 extra line */
    erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    switch(key) {
        case ARROW_LEFT:
            if (E.cx != 0) {
                E.cx--;
            } else if (E.cy > 0) {
                /* if pressed '<-' on extreme left, go to previous line's end */ 
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
            break;
        case ARROW_RIGHT:
            /* Limits x movement, allows only 1 extra character */
            if (row && E.cx < row->size) {
                E.cx++;
            } else if (row && E.cx == row->size) {
                /* if pressed '->' on extreme right, go to previous line's start */ /* i`row` makes sure not EoF */
                E.cy++;
                E.cx = 0;
            }
            break;
        case ARROW_UP:
            if (E.cy != 0) {
                E.cy--;
            }
            break;
        case ARROW_DOWN:
            if (E.cy < E.numrows) {
                E.cy++;
            }
            break;
    }

    /* Code added to tackle situation where -
     * we reach End of a line and then press down-key and the next line is shorter, we need to adjust len */
    /* We set `row` again, as since E.cy could point to diff line.
     * We then set `E.cx` to end of line if E.cx is to the right of the end of that line. */
    row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    int rowlen = row ? row->size : 0;
    if (E.cx > rowlen) {
        E.cx = rowlen;
    }
}

/* deals with mapping keys to editor functions at higher level */
void editorProcessKeypress() {
    static int quit_times = TEXT_IT_QUIT_TIMES;

    int c = editorReadKey();

    switch(c) {
        case '\r':      /* ENTER key */
            editorInsertNewline();
            break;

        case CTRL_KEY('q'):
            if (E.dirty && quit_times > 0) {
                editorSetStatusMessage("WARNING!!! File has unsaved changes. " 
                    "Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }
            /* clear the screen and position the cursor before exiting */
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;

        case CTRL_KEY('s'):
            editorSave();
            break;

        case HOME_KEY: 
            E.cx = 0;
            break;

        case END_KEY:
            if (E.cy < E.numrows)
                E.cx = E.row[E.cy].size;    /* reaches to the end of line */
            break;

        case CTRL_KEY('f'):
            editorFind();
            break;

        case BACKSPACE:
        case CTRL_KEY('h'):     /* control code 8 - equivalent to backspace. (olden days) */
        case DEL_KEY:
            if (c == DEL_KEY)   editorMoveCursor(ARROW_RIGHT);  /* DELETE is equivalent to: Go right and press backspace */
            editorDelChar();
            break;

        case PAGE_UP:
        case PAGE_DOWN:
            { /* we create a code block bcz we can't declare variables 
               * directly inside a switch statement */
                if (c == PAGE_UP) { 
                /* scroll up and down 1 entire page. That is, 1 page is scrolled completely, 
                 * previously it was going only to the top/bottom of the screen */
                    E.cy = E.rowoff;
                } else if (c == PAGE_DOWN) {
                    E.cy = E.rowoff + E.screenrows - 1;
                    if (E.cy > E.numrows) E.cy = E.numrows;
                }

                int times = E.screenrows;
                while (times--)
                    editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            }
            break;

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;

        case CTRL_KEY('l'):  /* Used to refresh the screen in terminal programs, 
                              * in our prog, it refreshes after any keypress */
        case '\x1b':         /* ESCAPE key */ 
            break;
                             /* We ignore ESCAPE because there are many key escape sequences that we aren't handling, 
                              * and the way we wrote `editorReadKey()`, pressing those keys will be equivalent to 
                              * pressing the ESCAPE key */

        default:
            editorInsertChar(c);
            break;
    }

    quit_times = TEXT_IT_QUIT_TIMES;  /* declared as `static`, so should be reset in case any other key is pressed */
}



/*** init ***/

/* to initialize all the fields in the `E` struct */
void initEditor() {
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.numrows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;    
    /* NULL => no filetype for current file, and no syntax highlighting should be done */

    if (getWindowSize(&E.screenrows, &E.screencols) == -1)  die("initEditor");
    E.screenrows -= 2;  /* making space for status bar and msg bar at the bottom of the screen */
}

int main(int argc, char *argv[]) {
    enableRawMode();
    initEditor();

    if (argc >= 2) {
        editorOpen(argv[1]);   /* for opening and reading a file from disk */
    }

    editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (1) {
        editorRefreshScreen();
        editorProcessKeypress();
    }

    return 0;
}
