/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
#include <stdio.h>
#include "sys.h"
#include <windowsx.h>
#include "resource.h"
#include "erl_version.h"
#include <commdlg.h>
#include <commctrl.h>

#define WM_CONTEXT      (0x0401)
#define WM_CONBEEP      (0x0402)

#define USER_KEY "Software\\Ericsson\\Erlang\\"##ERLANG_VERSION

#define FRAME_HEIGHT ((2*GetSystemMetrics(SM_CYEDGE))+(2*GetSystemMetrics(SM_CYFRAME))+GetSystemMetrics(SM_CYCAPTION))
#define FRAME_WIDTH  (2*GetSystemMetrics(SM_CXFRAME)+(2*GetSystemMetrics(SM_CXFRAME))+GetSystemMetrics(SM_CXVSCROLL))

#define LINE_LENGTH 80
#define COL(_l) ((_l) % LINE_LENGTH)
#define LINE(_l) ((_l) / LINE_LENGTH)

/*
 * XXX There is no escape to send a character 0x80.  Fortunately, 
 * the ttsl driver currently replaces 0x80 with an octal sequence.
 */
#define SET_CURSOR (0x80)

typedef struct ScreenLine_s {
    struct ScreenLine_s* next;
    struct ScreenLine_s* prev;
    int width;
    char text[LINE_LENGTH];
} ScreenLine_t;

extern byte *lbuf;		/* The current line buffer */
extern int llen;		/* The current line length */
extern byte *lc;		/* The current character pointer */
extern int lpos;

HANDLE console_input_event;
HANDLE console_thread = NULL;

#define BUFSIZE 4096
#define MAXBUFSIZE 32768
typedef struct {
    unsigned char *data; 
    int size;
    int wrPos;
    int rdPos;
} buffer_t;
static buffer_t inbuf;
static buffer_t outbuf;

static CHOOSEFONT cf;

static char szFrameClass[] = "FrameClass";
static char szClientClass[] = "ClientClass";
static HWND hFrameWnd;
static HWND hClientWnd;
static HWND hTBWnd;
static HWND hComboWnd;
static HANDLE console_input;
static HANDLE console_output;
static int cxChar,cyChar;
static int cxClient,cyClient;
static int cyToolBar;
static int iVscrollPos,iHscrollPos;
static int iVscrollMax,iHscrollMax;
static int nBufLines;
static int cur_x;
static int cur_y;
static ScreenLine_t *buffer_top,*buffer_bottom;
static ScreenLine_t* cur_line;
static POINT editBeg,editEnd;
static BOOL fSelecting = FALSE;
static BOOL fTextSelected = FALSE;
static HKEY key;
static BOOL has_key = FALSE;
static LOGFONT logfont;
static DWORD fgColor;
static DWORD bkgColor;
static FILE *logfile = NULL;

static int lines_to_save = 1000; /* Maximum number of screen lines to save. */

static DWORD WINAPI ConThreadInit(LPVOID param);
LRESULT CALLBACK ClientWndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK FrameWndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK AboutDlgProc(HWND hDlg, UINT iMsg, WPARAM wParam, LPARAM lParam);
static ScreenLine_t *ConNewLine(void);
static void DeleteTopLine(void);
static void ensure_line_below(void);
static ScreenLine_t *GetLineFromY(int y);
static void LoadUserPreferences(void);
static void SaveUserPreferences(void);
static void set_scroll_info(HWND hwnd);
static void ConCarriageFeed(void);
static void ConScrollScreen(void);
static BOOL ConChooseFont(HWND hwnd);
static void ConFontInitialize(HWND hwnd);
static void ConSetFont(HWND hwnd);
static void ConChooseColor(HWND hwnd);
static void DrawSelection(HWND hwnd, POINT pt1, POINT pt2);
static void OnEditCopy(HWND hwnd);
static void OnEditPaste(HWND hwnd);
static void OnEditSelAll(HWND hwnd);
static void GetFileName(HWND hwnd, char *pFile);
static void OpenLogFile(HWND hwnd);
static void CloseLogFile(HWND hwnd);
static void LogFileWrite(unsigned char *buf, int n);
static int write_inbuf(unsigned char *data, int n);
static void init_buffers(void);
static void AddToCmdHistory(void);
static int write_outbuf(unsigned char *data, int nbytes);
static void ConDrawText(HWND hwnd);
static BOOL (WINAPI *ctrl_handler)(DWORD);
static HWND InitToolBar(HWND hwndParent); 
static char* window_title(void);

void
ConInit(void)
{
    DWORD tid;
    
    console_input = CreateSemaphore(NULL, 0, 1, NULL);
    console_output = CreateSemaphore(NULL, 0, 1, NULL);
    console_input_event = CreateManualEvent(FALSE);
    console_thread = (HANDLE *) _beginthreadex(NULL, 0, ConThreadInit,
					       0, 0, &tid);
}

void
ConWaitForExit(void)
{
    ConPrintf("\n\nAbnormal termination\n");
    WaitForSingleObject(console_thread, INFINITE);
}

void ConSetCtrlHandler(BOOL (WINAPI *handler)(DWORD))
{
    ctrl_handler = handler;
}

int ConPutChar(int c)
{
    char sbuf[1];

    sbuf[0] = c;
    write_outbuf(sbuf, 1);
    return 1;
}

void ConSetCursor(int from, int to)
{   unsigned char cmd[9];
    int *p;

    cmd[0] = SET_CURSOR;
    /*
     * XXX Expect trouble on CPUs which don't allow misaligned read and writes.
     */
    p = (int *)&cmd[1];
    *p++ = from;
    *p = to;
    write_outbuf(cmd, 9);
}

void ConPrintf(char *format, ...)
{
    va_list va;

    va_start(va, format);
    ConVprintf(format, va);
    va_end(va);
}

void ConVprintf(char *format, va_list va)
{
    char sbuf[1024];

    vsprintf(sbuf, format, va);
    write_outbuf(sbuf, strlen(sbuf));
}

void ConBeep(void)
{
    SendMessage(hClientWnd, WM_CONBEEP, 0L, 0L);
}

int ConReadInput(unsigned char *data, int nbytes)
{
    unsigned char *buf;
    int nread;
    WaitForSingleObject(console_input,INFINITE);
    nread = nbytes = min(nbytes,inbuf.wrPos-inbuf.rdPos);
    buf = &inbuf.data[inbuf.rdPos];
    inbuf.rdPos += nread;
    while (nread--) 
        *data++ = *buf++;
    if (inbuf.rdPos >= inbuf.wrPos) {
        inbuf.rdPos = 0;
        inbuf.wrPos = 0;
        ResetEvent(console_input_event);
    }
    ReleaseSemaphore(console_input,1,NULL);
    return nbytes;
}

int ConGetKey(void)
{
    char c;
    WaitForSingleObject(console_input,INFINITE);
    ResetEvent(console_input_event);
    inbuf.rdPos = inbuf.wrPos = 0;
    ReleaseSemaphore(console_input,1,NULL);
    WaitForSingleObject(console_input_event,INFINITE);
    ConReadInput(&c, 1);
    return c;
}

static HINSTANCE hInstance;
extern HMODULE beam_module;

static DWORD WINAPI
ConThreadInit(LPVOID param)
{
    MSG msg;
    WNDCLASSEX wndclass;
    int iCmdShow;
    STARTUPINFO StartupInfo;
    HACCEL hAccel;

    /*DebugBreak();*/
    hInstance = GetModuleHandle(NULL);
    StartupInfo.dwFlags = 0;
    GetStartupInfo(&StartupInfo);
    iCmdShow = StartupInfo.dwFlags & STARTF_USESHOWWINDOW ?
	StartupInfo.wShowWindow : SW_SHOWDEFAULT;

    LoadUserPreferences();

    /* frame window class */
    wndclass.cbSize	        = sizeof (wndclass);	
    wndclass.style          = CS_HREDRAW | CS_VREDRAW | CS_BYTEALIGNCLIENT;
    wndclass.lpfnWndProc    = FrameWndProc;
    wndclass.cbClsExtra     = 0;
    wndclass.cbWndExtra     = 0;
    wndclass.hInstance      = hInstance;
    wndclass.hIcon          = LoadIcon (hInstance, MAKEINTRESOURCE(1));
    wndclass.hCursor        = LoadCursor (NULL, IDC_ARROW);
    wndclass.hbrBackground  = NULL;
    wndclass.lpszMenuName   = NULL;
    wndclass.lpszClassName  = szFrameClass;
    wndclass.hIconSm	    = LoadIcon (hInstance, MAKEINTRESOURCE(1));
    RegisterClassEx (&wndclass);

    /* client window class */
    wndclass.cbSize	        = sizeof (wndclass);	
    wndclass.style          = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
    wndclass.lpfnWndProc    = ClientWndProc;
    wndclass.cbClsExtra     = 0;
    wndclass.cbWndExtra     = 0;
    wndclass.hInstance      = hInstance;
    wndclass.hIcon          = LoadIcon (hInstance, MAKEINTRESOURCE(1));
    wndclass.hCursor        = LoadCursor (NULL, IDC_ARROW);
    wndclass.hbrBackground  = CreateSolidBrush(bkgColor);
    wndclass.lpszMenuName   = NULL;
    wndclass.lpszClassName  = szClientClass;
    wndclass.hIconSm	    = LoadIcon (hInstance, MAKEINTRESOURCE(1));
    RegisterClassEx (&wndclass);

    InitCommonControls();
    init_buffers();

    nBufLines = 0;
    buffer_top = cur_line = ConNewLine();
    cur_line->next = buffer_bottom = ConNewLine();
    buffer_bottom->prev = cur_line;

    /* Create Frame Window */
    hFrameWnd = CreateWindowEx(0, szFrameClass, window_title(),
			       WS_OVERLAPPEDWINDOW,CW_USEDEFAULT,
			       CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT,	
			       NULL,LoadMenu(beam_module,MAKEINTRESOURCE(1)),
			       hInstance,NULL);

    SetWindowPos(hFrameWnd, NULL, 0, 0,
		 cxChar*LINE_LENGTH+FRAME_WIDTH+GetSystemMetrics(SM_CXVSCROLL),
		 cyChar*30+FRAME_HEIGHT, SWP_NOMOVE|SWP_NOZORDER);

    ShowWindow(hFrameWnd, iCmdShow);
    UpdateWindow(hFrameWnd);

    hAccel = LoadAccelerators(beam_module,MAKEINTRESOURCE(1));

    ReleaseSemaphore(console_input, 1, NULL);
    ReleaseSemaphore(console_output, 1, NULL);


    /* Main message loop */
    while (GetMessage (&msg, NULL, 0, 0))
    {
        if (!TranslateAccelerator(hFrameWnd,hAccel,&msg)) 
        {
            TranslateMessage (&msg);
            DispatchMessage (&msg);
        }
    }
    SaveUserPreferences();
    (*ctrl_handler)(CTRL_CLOSE_EVENT);
    return msg.wParam;
}

static LRESULT CALLBACK
FrameWndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
    RECT r;
    int cy,i,bufsize;
    unsigned char c;
    char buf[128];

    switch (iMsg) {         
    case WM_CREATE:
        /* client window creation */
        hClientWnd = CreateWindowEx(WS_EX_CLIENTEDGE, szClientClass, window_title(),
				    WS_CHILD|WS_VISIBLE|WS_VSCROLL|WS_HSCROLL,
				    CW_USEDEFAULT, CW_USEDEFAULT,
				    CW_USEDEFAULT, CW_USEDEFAULT,		
				    hwnd, (HMENU)0, hInstance, NULL);
        hTBWnd = InitToolBar(hwnd);
        UpdateWindow (hClientWnd);
        return 0;
    case WM_SIZE :
        if (IsWindowVisible(hTBWnd)) {
            SendMessage(hTBWnd,TB_AUTOSIZE,0,0L);
            GetWindowRect(hTBWnd,&r);
            cy = r.bottom-r.top;
        } else cy = 0;
        MoveWindow(hClientWnd,0,cy,LOWORD(lParam),HIWORD(lParam)-cy,TRUE);
        return 0;
    case WM_ERASEBKGND:
        return 1;
    case WM_SETFOCUS :
        CreateCaret(hClientWnd, NULL, cxChar, cyChar);
        SetCaretPos((cur_x)*cxChar, (cur_y-iVscrollPos)*cyChar);
        ShowCaret(hClientWnd);
        return 0;
    case WM_KILLFOCUS:
        HideCaret(hClientWnd);
        DestroyCaret();
        return 0;
    case WM_INITMENUPOPUP :
        if (lParam == 0)	/* File popup menu */
        {
            EnableMenuItem((HMENU)wParam, IDMENU_STARTLOG,
			   logfile ? MF_GRAYED : MF_ENABLED);
            EnableMenuItem((HMENU)wParam, IDMENU_STOPLOG,
			   logfile ? MF_ENABLED : MF_GRAYED);
            return 0;
	}
        else if (lParam == 1)	/* Edit popup menu */
        {
            EnableMenuItem((HMENU)wParam, IDMENU_COPY,
			   fTextSelected ? MF_ENABLED : MF_GRAYED);
            EnableMenuItem((HMENU)wParam, IDMENU_PASTE,
			   IsClipboardFormatAvailable(CF_TEXT) ? MF_ENABLED : MF_GRAYED);
            return 0;
        }
        else if (lParam == 3)	/* View popup menu */
        {
            CheckMenuItem((HMENU)wParam,IDMENU_TOOLBAR,
			  IsWindowVisible(hTBWnd) ? MF_CHECKED : MF_UNCHECKED);
            return 0;
        }
        break;
    case WM_NOTIFY: 
        switch (((LPNMHDR) lParam)->code) { 
        case TTN_NEEDTEXT: 
            {    
		LPTOOLTIPTEXT lpttt; 
		lpttt = (LPTOOLTIPTEXT) lParam; 
		lpttt->hinst = hInstance;
		/* check for combobox handle */
		if (lpttt->uFlags&TTF_IDISHWND) {
		    if ((lpttt->hdr.idFrom == (UINT) hComboWnd)) {
			lstrcpy(lpttt->lpszText,"Command History");
			break;
		    }
		}
		/* check for toolbar buttons */
		switch (lpttt->hdr.idFrom) { 
                case IDMENU_COPY: 
                    lstrcpy(lpttt->lpszText,"Copy (Ctrl+Ins)"); 
                    break; 
                case IDMENU_PASTE: 
                    lstrcpy(lpttt->lpszText,"Paste (Ctrl+V)"); 
                    break; 
		case IDMENU_FONT: 
                    lstrcpy(lpttt->lpszText,"Fonts"); 
                    break; 
		case IDMENU_ABOUT: 
                    lstrcpy(lpttt->lpszText,"Help"); 
                    break; 
		} 
	    }
        }
        break; 
    case WM_COMMAND:
	switch(LOWORD(wParam))
	{
	case IDMENU_STARTLOG:
            OpenLogFile(hwnd);
	    return 0;
	case IDMENU_STOPLOG:
            CloseLogFile(hwnd);
	    return 0;
	case IDMENU_EXIT:
	    SendMessage(hwnd, WM_CLOSE, 0, 0L);
	    return 0;
	case IDMENU_COPY:
	    if (fTextSelected)
                OnEditCopy(hClientWnd);
	    return 0;
	case IDMENU_PASTE:
	    OnEditPaste(hClientWnd);
	    return 0;
        case IDMENU_SELALL:
            OnEditSelAll(hClientWnd);
            return 0;
	case IDMENU_FONT:
	    if (ConChooseFont(hClientWnd)) {
		ConSetFont(hClientWnd);
	    }
	    SaveUserPreferences();
	    return 0;
        case IDMENU_SELECTBKG:
            ConChooseColor(hClientWnd);
	    SaveUserPreferences();
            return 0;
        case IDMENU_TOOLBAR:
            if (IsWindowVisible(hTBWnd))
                ShowWindow(hTBWnd,SW_HIDE);
            else
                ShowWindow(hTBWnd,SW_SHOW);
            GetClientRect(hwnd,&r);
            PostMessage(hwnd,WM_SIZE,0,MAKELPARAM(r.right,r.bottom));
            return 0;
        case IDMENU_ABOUT:
            DialogBox(beam_module,"AboutBox",hwnd,AboutDlgProc);
            return 0;
        case ID_COMBOBOX:
            switch (HIWORD(wParam)) {
            case CBN_SELENDOK:
                i = SendMessage(hComboWnd,CB_GETCURSEL,0,0);
                if (i != CB_ERR) {
                    buf[0] = 0x01; /* CTRL+A */
                    buf[1] = 0x0B; /* CTRL+K */
                    bufsize = SendMessage(hComboWnd,CB_GETLBTEXT,i,(LPARAM)&buf[2]);
                    if (bufsize != CB_ERR)
                        write_inbuf(buf,bufsize+2);
                    SetFocus(hwnd);
                }
                break;
            case CBN_SELENDCANCEL:
                break;
            }
        }
        break;
    case WM_KEYDOWN :
	switch (wParam) {
	case VK_UP: c = 'P'-'@'; break;
	case VK_DOWN : c = 'N'-'@'; break;
	case VK_RIGHT : c = 'F'-'@'; break;
	case VK_LEFT : c = 'B'-'@'; break;
	case VK_DELETE : c = 'D' -'@'; break;
	case VK_HOME : c = 'A'-'@'; break;
	case VK_END : c = 'E'-'@'; break;
        case VK_RETURN : AddToCmdHistory(); return 0;
	case VK_PRIOR :   /* PageUp */
	    PostMessage(hClientWnd, WM_VSCROLL, SB_PAGEUP, 0);
	    return 0;
	case VK_NEXT :   /* PageDown */
	    PostMessage(hClientWnd, WM_VSCROLL, SB_PAGEDOWN, 0);
	    return 0;
	default: return 0;
	}
        write_inbuf(&c, 1);
	return 0;
    case WM_CHAR:
	c = (unsigned char)wParam;
	if (c == 0x03) {
	    (*ctrl_handler)(CTRL_C_EVENT);
	    return 0;
	}
        write_inbuf(&c,1);
	return 0;
    case WM_CLOSE :
	break;
    case WM_DESTROY :
	PostQuitMessage(0);
	return 0;
    }
    return DefWindowProc(hwnd, iMsg, wParam, lParam);
}

static BOOL
Client_OnCreate(HWND hwnd, LPCREATESTRUCT lpCreateStruct)
{
    ConFontInitialize(hwnd);
    cur_x = cur_y = 0;
    iVscrollPos = 0;
    iHscrollPos = 0;
    return TRUE;
}

static void
Client_OnPaint(HWND hwnd)
{
    ScreenLine_t *pLine;
    int x,y,i,iTop,iBot;
    PAINTSTRUCT ps;
    RECT rcInvalid;
    HDC hdc;

    hdc = BeginPaint(hwnd, &ps);
    rcInvalid = ps.rcPaint;
    hdc = ps.hdc;
    iTop = max(0, iVscrollPos + rcInvalid.top/cyChar);
    iBot = min(nBufLines, iVscrollPos + rcInvalid.bottom/cyChar+1);
    pLine = GetLineFromY(iTop);
    for (i = iTop; i < iBot && pLine != NULL; i++) {
      	y = cyChar*(i-iVscrollPos);
        x = -cxChar*iHscrollPos;
	TextOut(hdc, x, y, &pLine->text[0], pLine->width);
        pLine = pLine->next;
    }
    if (fTextSelected || fSelecting) {
	DrawSelection(hwnd, editBeg, editEnd);
    }
    SetCaretPos((cur_x-iHscrollPos)*cxChar, (cur_y-iVscrollPos)*cyChar);
    EndPaint(hwnd, &ps);
}

static void
Client_OnSize(HWND hwnd, UINT state, int cx, int cy)
{
    cxClient = cx;
    cyClient = cy;
    set_scroll_info(hwnd);
    SetCaretPos((cur_x-iHscrollPos)*cxChar, (cur_y-iVscrollPos)*cyChar);
}

static void
Client_OnLButtonDown(HWND hwnd, BOOL fDoubleClick, int x, int y, UINT keyFlags)
{
    SetFocus(GetParent(hwnd));	/* In case combobox steals the focus */
    if (fTextSelected) {
	DrawSelection(hwnd, editBeg, editEnd);
    }
    fTextSelected = FALSE;
    editEnd.x = editBeg.x = x/cxChar + iHscrollPos;
    editEnd.y = editBeg.y = y/cyChar + iVscrollPos;
    editEnd.x = editBeg.x;
    editEnd.y = editBeg.y + 1;
    fSelecting = TRUE;
    SetCapture(hwnd);
}

static void
Client_OnLButtonUp(HWND hwnd, int x, int y, UINT keyFlags)
{
    if (fSelecting && editBeg.x != editEnd.x && editBeg.y != editEnd.y) {
	fTextSelected = TRUE;
    }
    fSelecting = FALSE;
    ReleaseCapture();
}

static void
Client_OnMouseMove(HWND hwnd, int x, int y, UINT keyFlags)
{
    if (fSelecting) {
	POINT pt;
	POINT from;
	POINT to;

	pt.x = x/cxChar + iHscrollPos;
	pt.y = y/cyChar + iVscrollPos + 1;

	from.x = editEnd.x;
	from.y = editBeg.y;
	to.x = pt.x;
	to.y = editEnd.y;
	DrawSelection(hwnd, from, to);

	from.x = editBeg.x;
	from.y = editEnd.y;
	to.x = pt.x;
	to.y = pt.y;
	DrawSelection(hwnd, from, to);
	editEnd.x = x/cxChar + iHscrollPos;
	editEnd.y = y/cyChar + iVscrollPos + 1;
    }
}

static void
Client_OnVScroll(HWND hwnd, HWND hwndCtl, UINT code, int pos)
{
    int iVscroll;

    switch(code) {
    case SB_LINEDOWN:
	iVscroll = 1;
	break;
    case SB_LINEUP:
	iVscroll = -1;
	break;
    case SB_PAGEDOWN:
	iVscroll = max(1, cyClient/cyChar);
	break;
    case SB_PAGEUP:
	iVscroll = min(-1, -cyClient/cyChar);
	break;
    case SB_THUMBTRACK:
	iVscroll = pos - iVscrollPos;
	break;
    default:
	iVscroll = 0;
    }
    iVscroll = max(-iVscrollPos, min(iVscroll, iVscrollMax-iVscrollPos));
    if (iVscroll != 0) {
	iVscrollPos += iVscroll;
	ScrollWindowEx(hwnd, 0, -cyChar*iVscroll, NULL, NULL,
		       NULL, NULL, SW_ERASE | SW_INVALIDATE);
	SetScrollPos(hwnd, SB_VERT, iVscrollPos, TRUE);
	iVscroll = GetScrollPos(hwnd, SB_VERT);
	UpdateWindow(hwnd);
    }
}

static void
Client_OnHScroll(HWND hwnd, HWND hwndCtl, UINT code, int pos)
{
    int iHscroll, curCharWidth = cxClient/cxChar;

    switch(code) {
    case SB_LINEDOWN:
	iHscroll = 1;
	break;
    case SB_LINEUP:
	iHscroll = -1;
	break;
    case SB_PAGEDOWN:
	iHscroll = max(1,curCharWidth-1);
	break;
    case SB_PAGEUP:
	iHscroll = min(-1,-(curCharWidth-1));
	break;
    case SB_THUMBTRACK:
	iHscroll = pos - iHscrollPos;
	break;
    default:
	iHscroll = 0;
    }
    iHscroll = max(-iHscrollPos, min(iHscroll, iHscrollMax-iHscrollPos-(curCharWidth-1)));
    if (iHscroll != 0) {
	iHscrollPos += iHscroll;
	ScrollWindow(hwnd, -cxChar*iHscroll, 0, NULL, NULL);
	SetScrollPos(hwnd, SB_HORZ, iHscrollPos, TRUE);
	UpdateWindow(hwnd);
    }
}

static LRESULT CALLBACK
ClientWndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
    switch (iMsg) {
	HANDLE_MSG(hwnd, WM_CREATE, Client_OnCreate);
	HANDLE_MSG(hwnd, WM_SIZE, Client_OnSize);
	HANDLE_MSG(hwnd, WM_PAINT, Client_OnPaint);
	HANDLE_MSG(hwnd, WM_LBUTTONDOWN, Client_OnLButtonDown);
	HANDLE_MSG(hwnd, WM_LBUTTONUP, Client_OnLButtonUp);
	HANDLE_MSG(hwnd, WM_MOUSEMOVE, Client_OnMouseMove);
	HANDLE_MSG(hwnd, WM_VSCROLL, Client_OnVScroll);
	HANDLE_MSG(hwnd, WM_HSCROLL, Client_OnHScroll);
    case WM_CONBEEP:
        if (0) Beep(440, 400);
        return 0;
    case WM_CONTEXT:
        ConDrawText(hwnd);
        return 0;
    case WM_CLOSE:
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProc (hwnd, iMsg, wParam, lParam);
}

static void
LoadUserPreferences(void) 
{
    int size;
    int res;
    int type;

    /* default prefs */
    GetObject(GetStockObject(SYSTEM_FIXED_FONT),sizeof(LOGFONT),(PSTR)&logfont);
    fgColor = GetSysColor(COLOR_WINDOWTEXT);
    bkgColor = GetSysColor(COLOR_WINDOW);

    if (RegCreateKeyEx(HKEY_CURRENT_USER, USER_KEY, 0, 0,
		       REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL,
		       &key, &res) != ERROR_SUCCESS)
        return;
    if (res == REG_CREATED_NEW_KEY) {
	has_key = TRUE;
	SaveUserPreferences();
    } else {
        size = sizeof(logfont);
        res = RegQueryValueEx(key,"Font",NULL,&type,(LPBYTE)&logfont,&size);
        size = sizeof(fgColor);
        res = RegQueryValueEx(key,"FgColor",NULL,&type,(LPBYTE)&fgColor,&size);
        size = sizeof(bkgColor);
        res = RegQueryValueEx(key,"BkColor",NULL,&type,(LPBYTE)&bkgColor,&size);
    }
}

static void
SaveUserPreferences(void)
{
    if (has_key == TRUE) {
        RegSetValueEx(key,"Font",0,REG_BINARY,(CONST BYTE *)&logfont,sizeof(LOGFONT));
        RegSetValueEx(key,"FgColor",0,REG_DWORD,(CONST BYTE *)&fgColor,sizeof(fgColor));
        RegSetValueEx(key,"BkColor",0,REG_DWORD,(CONST BYTE *)&bkgColor,sizeof(bkgColor));
    }
}


static void
set_scroll_info(HWND hwnd)
{
    SCROLLINFO info;
    int hScrollBy;
    /*
     * Set vertical scrolling range and scroll box position.
     */

    iVscrollMax = nBufLines-1;
    iVscrollPos = min(iVscrollPos, iVscrollMax);
    info.cbSize = sizeof(info); 
    info.fMask = SIF_PAGE|SIF_RANGE|SIF_POS;
    info.nMin = 0;
    info.nPos = iVscrollPos; 
    info.nPage = min(cyClient/cyChar, iVscrollMax);
    info.nMax = iVscrollMax;
    SetScrollInfo(hwnd, SB_VERT, &info, TRUE);

    /*
     * Set horizontal scrolling range and scroll box position.
     */ 

    iHscrollMax = LINE_LENGTH-1;
    hScrollBy = max(0, (iHscrollPos - (iHscrollMax-cxClient/cxChar))*cxChar);
    iHscrollPos = min(iHscrollPos, iHscrollMax);
    info.nPos = iHscrollPos; 
    info.nPage = cxClient/cxChar;
    info.nMax = iHscrollMax;
    SetScrollInfo(hwnd, SB_HORZ, &info, TRUE);
    /*ScrollWindow(hwnd, hScrollBy, 0, NULL, NULL);*/
}


static void
ensure_line_below(void)
{
    if (cur_line->next == NULL) {
	if (nBufLines >= lines_to_save) {
	    ScreenLine_t* pLine = buffer_top->next;
	    free(buffer_top);
	    buffer_top = pLine;
	    buffer_top->prev = NULL;
	    nBufLines--;
	}
	cur_line->next = ConNewLine();
	cur_line->next->prev = cur_line;
	buffer_bottom = cur_line;
	set_scroll_info(hClientWnd);
    }
}

static ScreenLine_t*
ConNewLine(void)
{
    ScreenLine_t *pLine;

    pLine = (ScreenLine_t *)malloc(sizeof(ScreenLine_t));
    if (!pLine)
	return NULL;
    pLine->width = 0;
    pLine->prev = pLine->next = NULL;
    nBufLines++;
    return pLine;
}

static ScreenLine_t*
GetLineFromY(int y)
{
    ScreenLine_t *pLine = buffer_top;
    int i;

    for (i = 0; i < nBufLines && pLine != NULL; i++) {
        if (i == y)
	    return pLine;
        pLine  = pLine->next;        
    }
    return NULL;
}    

void ConCarriageFeed()
{
    cur_x = 0;
    ensure_line_below();
    cur_line = cur_line->next;
    if (cur_y < nBufLines-1) {
	cur_y++;
    } else if (iVscrollPos > 0) {
	iVscrollPos--;
    }
}

/*
 * Scroll screen if cursor is not visible.
 */
static void
ConScrollScreen(void)
{
    if (cur_y >= iVscrollPos + cyClient/cyChar) {
	int iVscroll;

	iVscroll = cur_y - iVscrollPos - cyClient/cyChar + 1;
	iVscrollPos += iVscroll;
	ScrollWindowEx(hClientWnd, 0, -cyChar*iVscroll, NULL, NULL,
		     NULL, NULL, SW_ERASE | SW_INVALIDATE);
	SetScrollPos(hClientWnd, SB_VERT, iVscrollPos, TRUE);
	UpdateWindow(hClientWnd);
    }
}

static void
DrawSelection(HWND hwnd, POINT pt1, POINT pt2)
{
    HDC hdc;
    int width,height;
    pt1.y -= iVscrollPos;
    pt2.y -= iVscrollPos;
    pt1.x -= iHscrollPos;
    pt2.x -= iHscrollPos;
    pt1.x *= cxChar;
    pt2.x *= cxChar;
    pt1.y *= cyChar;
    pt2.y *= cyChar;
    width = pt2.x-pt1.x;
    height = pt2.y - pt1.y;
    hdc = GetDC(hwnd);
    PatBlt(hdc,pt1.x,pt1.y,width,height,DSTINVERT);
    ReleaseDC(hwnd,hdc);
}

static void
OnEditCopy(HWND hwnd)
{
    /* copy selected text to clipboard */
    RECT rc;
    int size,i,j;
    HGLOBAL hMem;
    char *pMem;
    ScreenLine_t *pLine;
    				
    rc.top = min(editBeg.y,editEnd.y);
    rc.bottom = max(editBeg.y,editEnd.y);
    rc.left = min(editBeg.x,editEnd.x);
    rc.right = max(editBeg.x,editEnd.x);
    size = (rc.bottom-rc.top) * (rc.right-rc.left+3) + 1;
    hMem = GlobalAlloc(GHND, size);
    pMem = GlobalLock(hMem);
    
    pLine = GetLineFromY(rc.top);
    for (i = rc.top; i < rc.bottom && pLine != NULL; i++) {
	for (j=rc.left ; j<rc.right; j++) {
	    if (j >= pLine->width)
		break;
	    *pMem++ = pLine->text[j];
	}
        if (i < rc.bottom-1) {
	    *pMem++ = '\r';
	    *pMem++ = '\n';
	}
	pLine = pLine->next;
    }
				
    OpenClipboard(hwnd);
    EmptyClipboard();
    SetClipboardData(CF_TEXT,hMem);
    CloseClipboard();
}

static void
OnEditPaste(HWND hwnd)
{
    HANDLE hClipMem;
    char *pClipMem,*pMem,*pMem2;
    if (!OpenClipboard(hwnd))
	return;
    if ((hClipMem = GetClipboardData(CF_TEXT)) != NULL) {
        pClipMem = GlobalLock(hClipMem);
        pMem = (char *)malloc(GlobalSize(hClipMem));
        pMem2 = pMem;
        while ((*pMem2 = *pClipMem) != '\0') {
            if (*pClipMem == '\r')
                *pMem2 = '\n';
            ++pMem2;
	    ++pClipMem;
        }
        GlobalUnlock(hClipMem);
        write_inbuf(pMem, strlen(pMem));
    }
    CloseClipboard();
}

static void
OnEditSelAll(HWND hwnd)
{
    editBeg.x = 0;
    editBeg.y = 0;
    editEnd.x = LINE_LENGTH-1;
    editEnd.y = cur_y;
    fTextSelected = TRUE;
    InvalidateRect(hwnd, NULL, TRUE);
}

UINT APIENTRY CFHookProc(HWND hDlg,UINT iMsg,WPARAM wParam,LPARAM lParam)
{
    /* Hook procedure for font dialog box */
    HWND hOwner;
    RECT rc,rcOwner,rcDlg;
    switch (iMsg) {
    case WM_INITDIALOG:
        /* center dialogbox within its owner window */
        if ((hOwner = GetParent(hDlg)) == NULL) 
            hOwner = GetDesktopWindow(); 
        GetWindowRect(hOwner, &rcOwner); 
        GetWindowRect(hDlg, &rcDlg); 
        CopyRect(&rc, &rcOwner); 
        OffsetRect(&rcDlg, -rcDlg.left, -rcDlg.top); 
        OffsetRect(&rc, -rc.left, -rc.top); 
        OffsetRect(&rc, -rcDlg.right, -rcDlg.bottom); 
        SetWindowPos(hDlg,HWND_TOP,rcOwner.left + (rc.right / 2), 
		     rcOwner.top + (rc.bottom / 2),0,0,SWP_NOSIZE); 
        return 1;
    default:
        break;
    }
    return 0;			/* Let the default procedure process the message */
}

static BOOL
ConChooseFont(HWND hwnd)
{
    HDC hdc;
    hdc = GetDC(hwnd);
    cf.lStructSize = sizeof(CHOOSEFONT);
    cf.hwndOwner = hwnd;
    cf.hDC = NULL;
    cf.lpLogFont = &logfont;
    cf.iPointSize = 0;
    cf.Flags = CF_INITTOLOGFONTSTRUCT|CF_SCREENFONTS|CF_FIXEDPITCHONLY|CF_EFFECTS|CF_ENABLEHOOK;
    cf.rgbColors = GetTextColor(hdc);
    cf.lCustData = 0L;
    cf.lpfnHook = CFHookProc;
    cf.lpTemplateName = NULL;
    cf.hInstance = NULL;
    cf.lpszStyle = NULL;
    cf.nFontType = 0;
    cf.nSizeMin = 0;
    cf.nSizeMax = 0;
    ReleaseDC(hwnd,hdc);
    return ChooseFont(&cf);
}

static void
ConFontInitialize(HWND hwnd)
{
    HDC hdc;
    TEXTMETRIC tm;
    HFONT hFont;

    hFont = CreateFontIndirect(&logfont);
    hdc = GetDC(hwnd);
    SelectObject(hdc, hFont); 
    SetTextColor(hdc,fgColor);
    SetBkColor(hdc,bkgColor);
    GetTextMetrics(hdc, &tm);
    cxChar = tm.tmAveCharWidth;
    cyChar = tm.tmHeight + tm.tmExternalLeading;
    ReleaseDC(hwnd, hdc);
}

static void
ConSetFont(HWND hwnd)
{
    HDC hdc;
    TEXTMETRIC tm;
    HFONT hFontNew;
	
    hFontNew = CreateFontIndirect(&logfont); 
    hdc = GetDC(hwnd);
    DeleteObject(SelectObject(hdc, hFontNew));
    GetTextMetrics(hdc, &tm);
    cxChar = tm.tmAveCharWidth;
    cyChar = tm.tmHeight + tm.tmExternalLeading;
    fgColor = cf.rgbColors;
    SetTextColor(hdc,fgColor);
    ReleaseDC(hwnd, hdc);
    set_scroll_info(hwnd);
    HideCaret(hwnd);
    if (DestroyCaret()) {
        CreateCaret(hwnd, NULL, cxChar, cyChar);
        SetCaretPos((cur_x-iHscrollPos)*cxChar, (cur_y-iVscrollPos)*cyChar); 
    }
    ShowCaret(hwnd);
    InvalidateRect(hwnd, NULL, TRUE);
}

UINT APIENTRY
CCHookProc(HWND hDlg,UINT iMsg,WPARAM wParam,LPARAM lParam)
{
    /* Hook procedure for choose color dialog box */
    HWND hOwner;
    RECT rc,rcOwner,rcDlg;
    switch (iMsg) {
    case WM_INITDIALOG:
        /* center dialogbox within its owner window */
        if ((hOwner = GetParent(hDlg)) == NULL) 
            hOwner = GetDesktopWindow(); 
        GetWindowRect(hOwner, &rcOwner); 
        GetWindowRect(hDlg, &rcDlg); 
        CopyRect(&rc, &rcOwner); 
        OffsetRect(&rcDlg, -rcDlg.left, -rcDlg.top); 
        OffsetRect(&rc, -rc.left, -rc.top); 
        OffsetRect(&rc, -rcDlg.right, -rcDlg.bottom); 
        SetWindowPos(hDlg,HWND_TOP,rcOwner.left + (rc.right / 2), 
		     rcOwner.top + (rc.bottom / 2),0,0,SWP_NOSIZE); 
        return 1;
    default:
        break;
    }
    return 0;			/* Let the default procedure process the message */
}

void ConChooseColor(HWND hwnd)
{
    CHOOSECOLOR cc;                 
    static COLORREF acrCustClr[16];                      
    HBRUSH hbrush;
    HDC hdc;

    /* Initialize CHOOSECOLOR */
    ZeroMemory(&cc, sizeof(CHOOSECOLOR));
    cc.lStructSize = sizeof(CHOOSECOLOR);
    cc.hwndOwner = hwnd;
    cc.lpCustColors = (LPDWORD) acrCustClr;
    cc.rgbResult = bkgColor;
    cc.lpfnHook = CCHookProc;
    cc.Flags = CC_FULLOPEN|CC_RGBINIT|CC_SOLIDCOLOR|CC_ENABLEHOOK;
 
    if (ChooseColor(&cc)==TRUE) {
        bkgColor = cc.rgbResult;
        hdc = GetDC(hwnd);
        SetBkColor(hdc,bkgColor);
        ReleaseDC(hwnd,hdc);
        hbrush = CreateSolidBrush(bkgColor);
        DeleteObject((HBRUSH)SetClassLong(hClientWnd,GCL_HBRBACKGROUND,(LONG)hbrush));
        InvalidateRect(hwnd,NULL,TRUE);		
    }    
}

UINT APIENTRY OFNHookProc(HWND hwndDlg,UINT iMsg,WPARAM wParam,LPARAM lParam)
{
    /* Hook procedure for open file dialog box */
    HWND hOwner,hDlg;
    RECT rc,rcOwner,rcDlg;
    hDlg = GetParent(hwndDlg);
    switch (iMsg) {
    case WM_INITDIALOG:
        /* center dialogbox within its owner window */
        if ((hOwner = GetParent(hDlg)) == NULL) 
            hOwner = GetDesktopWindow(); 
        GetWindowRect(hOwner, &rcOwner); 
        GetWindowRect(hDlg, &rcDlg); 
        CopyRect(&rc, &rcOwner); 
        OffsetRect(&rcDlg, -rcDlg.left, -rcDlg.top); 
        OffsetRect(&rc, -rc.left, -rc.top); 
        OffsetRect(&rc, -rcDlg.right, -rcDlg.bottom); 
        SetWindowPos(hDlg,HWND_TOP,rcOwner.left + (rc.right / 2), 
		     rcOwner.top + (rc.bottom / 2),0,0,SWP_NOSIZE); 
        return 1;
    default:
        break;
    }
    return 0;			/* the let default procedure process the message */
}

static void
GetFileName(HWND hwnd, char *pFile)
{
    /* Open the File Open dialog box and */
    /* retrieve the file name            */
    OPENFILENAME ofn;
    CHAR szFilterSpec [128] = "logfiles (*.log)\0*.log\0" \
	"All files (*.*)\0*.*\0\0";
    #define MAXFILENAME 256
    char szFileName[MAXFILENAME];
    char szFileTitle[MAXFILENAME];

    /* these need to be filled in */
    strcpy(szFileName, "erlshell.log");   
    strcpy(szFileTitle, ""); /* must be NULL */

    ofn.lStructSize       = sizeof(OPENFILENAME);
    ofn.hwndOwner         = NULL;
    ofn.lpstrFilter       = szFilterSpec;
    ofn.lpstrCustomFilter = NULL;
    ofn.nMaxCustFilter    = 0;
    ofn.nFilterIndex      = 0;
    ofn.lpstrFile         = szFileName;
    ofn.nMaxFile          = MAXFILENAME;
    ofn.lpstrInitialDir   = NULL;
    ofn.lpstrFileTitle    = szFileTitle;
    ofn.nMaxFileTitle     = MAXFILENAME;
    ofn.lpstrTitle        = "Open logfile";
    ofn.lpstrDefExt       = "log";
    ofn.Flags             = OFN_CREATEPROMPT|OFN_HIDEREADONLY|OFN_EXPLORER|OFN_ENABLEHOOK;
    ofn.lpfnHook          = OFNHookProc;
   
    if (!GetOpenFileName ((LPOPENFILENAME)&ofn)){
        *pFile = '\0';
    } else {
        strcpy(pFile, ofn.lpstrFile);
    }
}

void OpenLogFile(HWND hwnd)
{
    /* open a file for logging */
    char filename[_MAX_PATH];

    GetFileName(hwnd, filename);
    if (filename[0] == '\0')
        return;
    if (NULL == (logfile = fopen(filename,"w")))
        return;
}

void CloseLogFile(HWND hwnd)
{
    /* close log file */
    fclose(logfile);
    logfile = NULL;
}

void LogFileWrite(unsigned char *buf, int nbytes)
{
    /* write to logfile */
    int from,to;
    while (nbytes-- > 0) {     
        switch (*buf) {
        case SET_CURSOR:
            buf++;
            from = *((int *)buf);
            buf+=4;
            to = *((int *)buf);
            buf+=3;
            nbytes-=8;
            fseek(logfile,to-from,SEEK_CUR);
            break;
        default:
            fputc(*buf,logfile);
            break;
        }
        buf++;
    }
}

static void
init_buffers(void)
{
    inbuf.data = (unsigned char *) malloc(BUFSIZE);
    outbuf.data = (unsigned char *) malloc(BUFSIZE);
    inbuf.size = BUFSIZE;
    inbuf.rdPos = inbuf.wrPos = 0;
    outbuf.size = BUFSIZE;
    outbuf.rdPos = outbuf.wrPos = 0;
}

static int
check_realloc(buffer_t *buf, int nbytes)
{
    if (buf->wrPos + nbytes >= buf->size) {
	if (buf->size > MAXBUFSIZE)
	    return 0;
	buf->size += nbytes + BUFSIZE;
	if (!(buf->data = (unsigned char *)realloc(buf->data, buf->size))) {
	    buf->size = buf->rdPos = buf->wrPos = 0;
	    return 0;
        }
    }
    return 1;
}

static int
write_inbuf(unsigned char *data, int nbytes)
{
    unsigned char *buf;
    int nwrite;
    WaitForSingleObject(console_input,INFINITE);
    if (!check_realloc(&inbuf,nbytes)) {
        ReleaseSemaphore(console_input,1,NULL);
        return -1;
    }
    buf = &inbuf.data[inbuf.wrPos];
    inbuf.wrPos += nbytes; 
    nwrite = nbytes;
    while (nwrite--) 
        *buf++ = *data++;
    SetEvent(console_input_event);
    ReleaseSemaphore(console_input,1,NULL);
    return nbytes;
}

static int 
write_outbuf(unsigned char *data, int nbytes)
{
    unsigned char *buf;
    int nwrite;

    WaitForSingleObject(console_output,INFINITE);
    if (!check_realloc(&outbuf, nbytes)) {
        ReleaseSemaphore(console_output,1,NULL);
        return -1;
    }
    if (outbuf.rdPos == outbuf.wrPos)
        PostMessage(hClientWnd, WM_CONTEXT, 0L, 0L);
    buf = &outbuf.data[outbuf.wrPos];
    outbuf.wrPos += nbytes; 
    nwrite = nbytes;
    while (nwrite--) 
        *buf++ = *data++;
    ReleaseSemaphore(console_output,1,NULL);
    return nbytes;
}

BOOL CALLBACK AboutDlgProc(HWND hDlg, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
    HWND hOwner;
    RECT rc,rcOwner,rcDlg;

    switch (iMsg) {
    case WM_INITDIALOG:
        /* center dialogbox within its owner window */
        if ((hOwner = GetParent(hDlg)) == NULL) 
            hOwner = GetDesktopWindow(); 
        GetWindowRect(hOwner, &rcOwner); 
        GetWindowRect(hDlg, &rcDlg); 
        CopyRect(&rc, &rcOwner); 
        OffsetRect(&rcDlg, -rcDlg.left, -rcDlg.top); 
        OffsetRect(&rc, -rc.left, -rc.top); 
        OffsetRect(&rc, -rcDlg.right, -rcDlg.bottom); 
        SetWindowPos(hDlg,HWND_TOP,rcOwner.left + (rc.right / 2), 
		     rcOwner.top + (rc.bottom / 2),0,0,SWP_NOSIZE); 
        SetDlgItemText(hDlg, ID_VERSIONSTRING,
		       "Erlang emulator version " ERLANG_VERSION); 
        return TRUE;
    case WM_COMMAND:
        switch (LOWORD(wParam)) {
        case IDOK:
        case IDCANCEL:
            EndDialog(hDlg,0);
            return TRUE;
        }
        break;
    }
    return FALSE;
}

static void
ConDrawText(HWND hwnd)
{
    int nbytes;
    int nchars;
    unsigned char *buf;
    int from, to;
    int dl;
    int dc;
    RECT rc;

    WaitForSingleObject(console_output, INFINITE);
    nchars = 0;
    nbytes = outbuf.wrPos - outbuf.rdPos;
    buf = &outbuf.data[outbuf.rdPos];
    if (logfile != NULL)
	LogFileWrite(buf, nbytes);

    /*
     * Don't draw any text in the window; just update the line buffers
     * and invalidate the appropriate part of the window.  The window
     * will be updated on the next WM_PAINT message.
     */

    while (nbytes-- > 0) {     
        switch (*buf) {
        case '\r':
            break;
        case '\n':
            if (nchars > 0) {
		rc.left = (cur_x - nchars - iHscrollPos) * cxChar;
		rc.right = rc.left + cxChar*nchars;
		rc.top = cyChar * (cur_y-iVscrollPos);
		rc.bottom = rc.top + cyChar;
		InvalidateRect(hwnd, &rc, TRUE);
                nchars = 0;
	    }
	    ConCarriageFeed();
            ConScrollScreen();
            break;
        case SET_CURSOR:
            if (nchars > 0) { 
		rc.left = (cur_x - nchars - iHscrollPos) * cxChar;
		rc.right = rc.left + cxChar*nchars;
		rc.top = cyChar * (cur_y-iVscrollPos);
		rc.bottom = rc.top + cyChar;
		InvalidateRect(hwnd, &rc, TRUE);
                nchars = 0;
            }
            buf++;
            from = *((int *)buf);
            buf += 4;
            to = *((int *)buf);
            buf += 4-1;
            nbytes -= 8;
	    dc = COL(to) - COL(from);
	    dl = LINE(to) - LINE(from);
	    cur_x += dc;
	    if (cur_x >= LINE_LENGTH) {
		cur_y++;
		ensure_line_below();
		cur_line = cur_line->next;
		cur_x -= LINE_LENGTH;
	    } else if (cur_x < 0) {
		cur_y--;
		cur_line = cur_line->prev;
		cur_x += LINE_LENGTH;
	    }
	    cur_y += dl;
	    if (dl > 0) {
		while (dl--) {
		    ensure_line_below();
		    cur_line = cur_line->next;
		}
	    } else {
		while (dl++) {
		    cur_line = cur_line->prev;
		}
	    }
            break;
        default:
            nchars++;
	    cur_line->text[cur_x] = *buf;
	    cur_x++;
            if (cur_x > cur_line->width)
		cur_line->width = cur_x; 
            if (cur_x  >= LINE_LENGTH) {
                if (nchars > 0) { 
                    rc.left = (cur_x - nchars - iHscrollPos) * cxChar;
                    rc.right = rc.left + cxChar*nchars;
                    rc.top = cyChar * (cur_y-iVscrollPos);
                    rc.bottom = rc.top + cyChar;
		    InvalidateRect(hwnd, &rc, TRUE);
                }
                ConCarriageFeed();
                nchars = 0;
            }
        }
        buf++;
    }
    if (nchars > 0) {
	rc.left = (cur_x - nchars - iHscrollPos) * cxChar;
	rc.right = rc.left + cxChar*nchars;
	rc.top = cyChar * (cur_y-iVscrollPos);
	rc.bottom = rc.top + cyChar;
	InvalidateRect(hwnd, &rc, TRUE);
    }
    ConScrollScreen();
    SetCaretPos((cur_x-iHscrollPos)*cxChar, (cur_y-iVscrollPos)*cyChar);
    outbuf.wrPos = outbuf.rdPos = 0;
    ReleaseSemaphore(console_output, 1, NULL);
}

static void
AddToCmdHistory(void)
{
    int i;
    int size;
    char *buf;
    char cmdBuf[128];

    if (llen != 0) {
	for (i = 0, size = 0; i < llen-1; i++) {
	    /*
	     * Find end of prompt.
	     */
	    if ((lbuf[i] == '>') && lbuf[i+1] == ' ') {
		buf = &lbuf[i+2];
		size = llen-i-2;
		break;
	    }
	}
	if (size > 0 && size < 128) {
	    strncpy(cmdBuf, buf, size);
	    cmdBuf[size] = 0;
	    SendMessage(hComboWnd,CB_INSERTSTRING,0,(LPARAM)cmdBuf);
	}
    }
}

static TBBUTTON tbb[] =
{
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          IDMENU_COPY,	TBSTATE_ENABLED, TBSTYLE_BUTTON, 0, 0, 0, 0,
    1,	        IDMENU_PASTE,	TBSTATE_ENABLED, TBSTYLE_BUTTON, 0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    2,	        IDMENU_FONT,	TBSTATE_ENABLED, TBSTYLE_BUTTON, 0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    3,	        IDMENU_ABOUT,	TBSTATE_ENABLED, TBSTYLE_BUTTON, 0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
};

static TBADDBITMAP tbbitmap =
{
    HINST_COMMCTRL, IDB_STD_SMALL_COLOR,
};

static HWND
InitToolBar(HWND hwndParent) 
{ 
    int x,y,cx;
    HWND hwndTB,hwndTT; 
    RECT r;
    TOOLINFO ti;

    /* Create toolbar window with tooltips */
    hwndTB = CreateWindowEx(0,TOOLBARCLASSNAME,(LPSTR)NULL,
			    WS_CHILD|CCS_TOP|WS_CLIPSIBLINGS|TBSTYLE_TOOLTIPS,
			    0,0,0,0,hwndParent,
			    (HMENU)2,hInstance,NULL); 
    SendMessage(hwndTB,TB_BUTTONSTRUCTSIZE,
		(WPARAM) sizeof(TBBUTTON),0); 
    /*tbbitmap.hInst = beam_module; 
    tbbitmap.nID   = 1;*/
    tbbitmap.hInst = NULL;
    tbbitmap.nID   = (UINT) LoadBitmap(beam_module, MAKEINTRESOURCE(1));
    SendMessage(hwndTB, TB_ADDBITMAP, (WPARAM) 4, 
		(WPARAM) &tbbitmap); 
    SendMessage(hwndTB,TB_ADDBUTTONS, (WPARAM) 32,
		(LPARAM) (LPTBBUTTON) tbb); 
    ShowWindow(hwndTB, SW_SHOW); 

    /* Create combobox window */
    SendMessage(hwndTB,TB_GETITEMRECT,0,(LPARAM)&r);
    x = r.left; y = r.top;
    SendMessage(hwndTB,TB_GETITEMRECT,23,(LPARAM)&r);
    cx = r.right - x + 1;
    hComboWnd = CreateWindow("combobox",NULL,WS_VSCROLL|WS_CHILD|WS_VISIBLE|CBS_DROPDOWNLIST,
			     x,y,cx,100,hwndParent,(HMENU)ID_COMBOBOX, hInstance,NULL);
    SetParent(hComboWnd,hwndTB);
    SendMessage(hComboWnd,WM_SETFONT,(WPARAM)GetStockObject(ANSI_FIXED_FONT),
		MAKELPARAM(1,0));

    /* Add tooltip for combo box */
    ZeroMemory(&ti,sizeof(TOOLINFO));
    ti.cbSize = sizeof(TOOLINFO);
    ti.uFlags = TTF_IDISHWND|TTF_CENTERTIP|TTF_SUBCLASS;
    ti.hwnd = hwndTB;;
    ti.uId = (UINT)hComboWnd;
    ti.lpszText = LPSTR_TEXTCALLBACK;
    hwndTT = (HWND)SendMessage(hwndTB,TB_GETTOOLTIPS,0,0);
    SendMessage(hwndTT,TTM_ADDTOOL,0,(LPARAM)&ti);

    return hwndTB; 
} 

static char*
window_title(void)
{
    char* title = getenv("ERL_WINDOW_TITLE");

    return title ? title : "Erlang";
}
