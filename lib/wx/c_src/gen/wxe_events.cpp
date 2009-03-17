/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd% 
*/

/***** This file is generated do not edit ****/ 

#include <wx/wx.h>
#include "../wxe_impl.h"

#include "wxe_macros.h"
#include "../wxe_events.h"

#include "../wxe_return.h"

wxeEtype::wxeEtype(const char *name, int Id) {eName = name;cID = Id;}

WX_DECLARE_HASH_MAP(int, wxeEtype*, wxIntegerHash, wxIntegerEqual, wxeETmap );

wxeETmap etmap; 

int wxeEventTypeFromAtom(char *etype_atom) {
  wxeETmap::iterator it;
  for(it = etmap.begin(); it != etmap.end(); ++it) {
       wxeEtype * value = it->second;
       if(strcmp(value->eName, etype_atom) == 0) { 
	   return it->first;
       }
  }   
  return -1; 
}

void initEventTable() 
{
 etmap[wxEVT_NULL] = new wxeEtype("null", 0);
 etmap[wxEVT_NAVIGATION_KEY] = new wxeEtype("navigation_key", 1);
 etmap[wxEVT_SASH_DRAGGED] = new wxeEtype("sash_dragged", 2);
 etmap[wxEVT_COMMAND_LIST_BEGIN_DRAG] = new wxeEtype("command_list_begin_drag", 3);
 etmap[wxEVT_COMMAND_LIST_BEGIN_RDRAG] = new wxeEtype("command_list_begin_rdrag", 3);
 etmap[wxEVT_COMMAND_LIST_BEGIN_LABEL_EDIT] = new wxeEtype("command_list_begin_label_edit", 3);
 etmap[wxEVT_COMMAND_LIST_END_LABEL_EDIT] = new wxeEtype("command_list_end_label_edit", 3);
 etmap[wxEVT_COMMAND_LIST_DELETE_ITEM] = new wxeEtype("command_list_delete_item", 3);
 etmap[wxEVT_COMMAND_LIST_DELETE_ALL_ITEMS] = new wxeEtype("command_list_delete_all_items", 3);
 etmap[wxEVT_COMMAND_LIST_KEY_DOWN] = new wxeEtype("command_list_key_down", 3);
 etmap[wxEVT_COMMAND_LIST_INSERT_ITEM] = new wxeEtype("command_list_insert_item", 3);
 etmap[wxEVT_COMMAND_LIST_COL_CLICK] = new wxeEtype("command_list_col_click", 3);
 etmap[wxEVT_COMMAND_LIST_COL_RIGHT_CLICK] = new wxeEtype("command_list_col_right_click", 3);
 etmap[wxEVT_COMMAND_LIST_COL_BEGIN_DRAG] = new wxeEtype("command_list_col_begin_drag", 3);
 etmap[wxEVT_COMMAND_LIST_COL_DRAGGING] = new wxeEtype("command_list_col_dragging", 3);
 etmap[wxEVT_COMMAND_LIST_COL_END_DRAG] = new wxeEtype("command_list_col_end_drag", 3);
 etmap[wxEVT_COMMAND_LIST_ITEM_SELECTED] = new wxeEtype("command_list_item_selected", 3);
 etmap[wxEVT_COMMAND_LIST_ITEM_DESELECTED] = new wxeEtype("command_list_item_deselected", 3);
 etmap[wxEVT_COMMAND_LIST_ITEM_RIGHT_CLICK] = new wxeEtype("command_list_item_right_click", 3);
 etmap[wxEVT_COMMAND_LIST_ITEM_MIDDLE_CLICK] = new wxeEtype("command_list_item_middle_click", 3);
 etmap[wxEVT_COMMAND_LIST_ITEM_ACTIVATED] = new wxeEtype("command_list_item_activated", 3);
 etmap[wxEVT_COMMAND_LIST_ITEM_FOCUSED] = new wxeEtype("command_list_item_focused", 3);
 etmap[wxEVT_COMMAND_LIST_CACHE_HINT] = new wxeEtype("command_list_cache_hint", 3);
 etmap[wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED] = new wxeEtype("command_notebook_page_changed", 4);
 etmap[wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGING] = new wxeEtype("command_notebook_page_changing", 4);
 etmap[wxEVT_DISPLAY_CHANGED] = new wxeEtype("display_changed", 5);
 etmap[wxEVT_ERASE_BACKGROUND] = new wxeEtype("erase_background", 6);
 etmap[wxEVT_CHAR] = new wxeEtype("char", 7);
 etmap[wxEVT_CHAR_HOOK] = new wxeEtype("char_hook", 7);
 etmap[wxEVT_KEY_DOWN] = new wxeEtype("key_down", 7);
 etmap[wxEVT_KEY_UP] = new wxeEtype("key_up", 7);
 etmap[wxEVT_DESTROY] = new wxeEtype("destroy", 8);
 etmap[wxEVT_CALENDAR_SEL_CHANGED] = new wxeEtype("calendar_sel_changed", 9);
 etmap[wxEVT_CALENDAR_DAY_CHANGED] = new wxeEtype("calendar_day_changed", 9);
 etmap[wxEVT_CALENDAR_MONTH_CHANGED] = new wxeEtype("calendar_month_changed", 9);
 etmap[wxEVT_CALENDAR_YEAR_CHANGED] = new wxeEtype("calendar_year_changed", 9);
 etmap[wxEVT_CALENDAR_DOUBLECLICKED] = new wxeEtype("calendar_doubleclicked", 9);
 etmap[wxEVT_CALENDAR_WEEKDAY_CLICKED] = new wxeEtype("calendar_weekday_clicked", 9);
 etmap[wxEVT_SCROLL_TOP] = new wxeEtype("scroll_top", 10);
 etmap[wxEVT_SCROLL_BOTTOM] = new wxeEtype("scroll_bottom", 10);
 etmap[wxEVT_SCROLL_LINEUP] = new wxeEtype("scroll_lineup", 10);
 etmap[wxEVT_SCROLL_LINEDOWN] = new wxeEtype("scroll_linedown", 10);
 etmap[wxEVT_SCROLL_PAGEUP] = new wxeEtype("scroll_pageup", 10);
 etmap[wxEVT_SCROLL_PAGEDOWN] = new wxeEtype("scroll_pagedown", 10);
 etmap[wxEVT_SCROLL_THUMBTRACK] = new wxeEtype("scroll_thumbtrack", 10);
 etmap[wxEVT_SCROLL_THUMBRELEASE] = new wxeEtype("scroll_thumbrelease", 10);
 etmap[wxEVT_SCROLL_CHANGED] = new wxeEtype("scroll_changed", 10);
 etmap[wxEVT_MENU_OPEN] = new wxeEtype("menu_open", 11);
 etmap[wxEVT_MENU_CLOSE] = new wxeEtype("menu_close", 11);
 etmap[wxEVT_MENU_HIGHLIGHT] = new wxeEtype("menu_highlight", 11);
 etmap[wxEVT_CONTEXT_MENU] = new wxeEtype("context_menu", 12);
 etmap[wxEVT_SHOW] = new wxeEtype("show", 13);
 etmap[wxEVT_SET_CURSOR] = new wxeEtype("set_cursor", 14);
 etmap[wxEVT_COMMAND_FONTPICKER_CHANGED] = new wxeEtype("command_fontpicker_changed", 15);
 etmap[wxEVT_SCROLLWIN_TOP] = new wxeEtype("scrollwin_top", 16);
 etmap[wxEVT_SCROLLWIN_BOTTOM] = new wxeEtype("scrollwin_bottom", 16);
 etmap[wxEVT_SCROLLWIN_LINEUP] = new wxeEtype("scrollwin_lineup", 16);
 etmap[wxEVT_SCROLLWIN_LINEDOWN] = new wxeEtype("scrollwin_linedown", 16);
 etmap[wxEVT_SCROLLWIN_PAGEUP] = new wxeEtype("scrollwin_pageup", 16);
 etmap[wxEVT_SCROLLWIN_PAGEDOWN] = new wxeEtype("scrollwin_pagedown", 16);
 etmap[wxEVT_SCROLLWIN_THUMBTRACK] = new wxeEtype("scrollwin_thumbtrack", 16);
 etmap[wxEVT_SCROLLWIN_THUMBRELEASE] = new wxeEtype("scrollwin_thumbrelease", 16);
 etmap[wxEVT_PAINT] = new wxeEtype("paint", 17);
 etmap[wxEVT_PAINT_ICON] = new wxeEtype("paint_icon", 17);
 etmap[wxEVT_SET_FOCUS] = new wxeEtype("set_focus", 18);
 etmap[wxEVT_KILL_FOCUS] = new wxeEtype("kill_focus", 18);
 etmap[wxEVT_MAXIMIZE] = new wxeEtype("maximize", 19);
 etmap[wxEVT_COMMAND_FILEPICKER_CHANGED] = new wxeEtype("command_filepicker_changed", 20);
 etmap[wxEVT_COMMAND_DIRPICKER_CHANGED] = new wxeEtype("command_dirpicker_changed", 20);
 etmap[wxEVT_SET_FOCUS] = new wxeEtype("set_focus", 21);
 etmap[wxEVT_KILL_FOCUS] = new wxeEtype("kill_focus", 21);
 etmap[wxEVT_DATE_CHANGED] = new wxeEtype("date_changed", 22);
 etmap[wxEVT_HELP] = new wxeEtype("help", 23);
 etmap[wxEVT_DETAILED_HELP] = new wxeEtype("detailed_help", 23);
 etmap[wxEVT_STC_CHANGE] = new wxeEtype("stc_change", 24);
 etmap[wxEVT_STC_STYLENEEDED] = new wxeEtype("stc_styleneeded", 24);
 etmap[wxEVT_STC_CHARADDED] = new wxeEtype("stc_charadded", 24);
 etmap[wxEVT_STC_SAVEPOINTREACHED] = new wxeEtype("stc_savepointreached", 24);
 etmap[wxEVT_STC_SAVEPOINTLEFT] = new wxeEtype("stc_savepointleft", 24);
 etmap[wxEVT_STC_ROMODIFYATTEMPT] = new wxeEtype("stc_romodifyattempt", 24);
 etmap[wxEVT_STC_KEY] = new wxeEtype("stc_key", 24);
 etmap[wxEVT_STC_DOUBLECLICK] = new wxeEtype("stc_doubleclick", 24);
 etmap[wxEVT_STC_UPDATEUI] = new wxeEtype("stc_updateui", 24);
 etmap[wxEVT_STC_MODIFIED] = new wxeEtype("stc_modified", 24);
 etmap[wxEVT_STC_MACRORECORD] = new wxeEtype("stc_macrorecord", 24);
 etmap[wxEVT_STC_MARGINCLICK] = new wxeEtype("stc_marginclick", 24);
 etmap[wxEVT_STC_NEEDSHOWN] = new wxeEtype("stc_needshown", 24);
 etmap[wxEVT_STC_PAINTED] = new wxeEtype("stc_painted", 24);
 etmap[wxEVT_STC_USERLISTSELECTION] = new wxeEtype("stc_userlistselection", 24);
 etmap[wxEVT_STC_URIDROPPED] = new wxeEtype("stc_uridropped", 24);
 etmap[wxEVT_STC_DWELLSTART] = new wxeEtype("stc_dwellstart", 24);
 etmap[wxEVT_STC_DWELLEND] = new wxeEtype("stc_dwellend", 24);
 etmap[wxEVT_STC_START_DRAG] = new wxeEtype("stc_start_drag", 24);
 etmap[wxEVT_STC_DRAG_OVER] = new wxeEtype("stc_drag_over", 24);
 etmap[wxEVT_STC_DO_DROP] = new wxeEtype("stc_do_drop", 24);
 etmap[wxEVT_STC_ZOOM] = new wxeEtype("stc_zoom", 24);
 etmap[wxEVT_STC_HOTSPOT_CLICK] = new wxeEtype("stc_hotspot_click", 24);
 etmap[wxEVT_STC_HOTSPOT_DCLICK] = new wxeEtype("stc_hotspot_dclick", 24);
 etmap[wxEVT_STC_CALLTIP_CLICK] = new wxeEtype("stc_calltip_click", 24);
 etmap[wxEVT_STC_AUTOCOMP_SELECTION] = new wxeEtype("stc_autocomp_selection", 24);
 etmap[wxEVT_SYS_COLOUR_CHANGED] = new wxeEtype("sys_colour_changed", 25);
 etmap[wxEVT_GRID_CELL_LEFT_CLICK] = new wxeEtype("grid_cell_left_click", 26);
 etmap[wxEVT_GRID_CELL_RIGHT_CLICK] = new wxeEtype("grid_cell_right_click", 26);
 etmap[wxEVT_GRID_CELL_LEFT_DCLICK] = new wxeEtype("grid_cell_left_dclick", 26);
 etmap[wxEVT_GRID_CELL_RIGHT_DCLICK] = new wxeEtype("grid_cell_right_dclick", 26);
 etmap[wxEVT_GRID_LABEL_LEFT_CLICK] = new wxeEtype("grid_label_left_click", 26);
 etmap[wxEVT_GRID_LABEL_RIGHT_CLICK] = new wxeEtype("grid_label_right_click", 26);
 etmap[wxEVT_GRID_LABEL_LEFT_DCLICK] = new wxeEtype("grid_label_left_dclick", 26);
 etmap[wxEVT_GRID_LABEL_RIGHT_DCLICK] = new wxeEtype("grid_label_right_dclick", 26);
 etmap[wxEVT_GRID_ROW_SIZE] = new wxeEtype("grid_row_size", 26);
 etmap[wxEVT_GRID_COL_SIZE] = new wxeEtype("grid_col_size", 26);
 etmap[wxEVT_GRID_RANGE_SELECT] = new wxeEtype("grid_range_select", 26);
 etmap[wxEVT_GRID_CELL_CHANGE] = new wxeEtype("grid_cell_change", 26);
 etmap[wxEVT_GRID_SELECT_CELL] = new wxeEtype("grid_select_cell", 26);
 etmap[wxEVT_GRID_EDITOR_SHOWN] = new wxeEtype("grid_editor_shown", 26);
 etmap[wxEVT_GRID_EDITOR_HIDDEN] = new wxeEtype("grid_editor_hidden", 26);
 etmap[wxEVT_GRID_EDITOR_CREATED] = new wxeEtype("grid_editor_created", 26);
 etmap[wxEVT_GRID_CELL_BEGIN_DRAG] = new wxeEtype("grid_cell_begin_drag", 26);
 etmap[wxEVT_PALETTE_CHANGED] = new wxeEtype("palette_changed", 27);
 etmap[wxEVT_UPDATE_UI] = new wxeEtype("update_ui", 28);
 etmap[wxEVT_SIZE] = new wxeEtype("size", 29);
 etmap[wxEVT_ICONIZE] = new wxeEtype("iconize", 30);
 etmap[wxEVT_CLOSE_WINDOW] = new wxeEtype("close_window", 31);
 etmap[wxEVT_END_SESSION] = new wxeEtype("end_session", 31);
 etmap[wxEVT_QUERY_END_SESSION] = new wxeEtype("query_end_session", 31);
 etmap[wxEVT_MOUSE_CAPTURE_CHANGED] = new wxeEtype("mouse_capture_changed", 32);
 etmap[wxEVT_LEFT_DOWN] = new wxeEtype("left_down", 33);
 etmap[wxEVT_LEFT_UP] = new wxeEtype("left_up", 33);
 etmap[wxEVT_MIDDLE_DOWN] = new wxeEtype("middle_down", 33);
 etmap[wxEVT_MIDDLE_UP] = new wxeEtype("middle_up", 33);
 etmap[wxEVT_RIGHT_DOWN] = new wxeEtype("right_down", 33);
 etmap[wxEVT_RIGHT_UP] = new wxeEtype("right_up", 33);
 etmap[wxEVT_MOTION] = new wxeEtype("motion", 33);
 etmap[wxEVT_ENTER_WINDOW] = new wxeEtype("enter_window", 33);
 etmap[wxEVT_LEAVE_WINDOW] = new wxeEtype("leave_window", 33);
 etmap[wxEVT_LEFT_DCLICK] = new wxeEtype("left_dclick", 33);
 etmap[wxEVT_MIDDLE_DCLICK] = new wxeEtype("middle_dclick", 33);
 etmap[wxEVT_RIGHT_DCLICK] = new wxeEtype("right_dclick", 33);
 etmap[wxEVT_MOUSEWHEEL] = new wxeEtype("mousewheel", 33);
 etmap[wxEVT_NC_LEFT_DOWN] = new wxeEtype("nc_left_down", 33);
 etmap[wxEVT_NC_LEFT_UP] = new wxeEtype("nc_left_up", 33);
 etmap[wxEVT_NC_MIDDLE_DOWN] = new wxeEtype("nc_middle_down", 33);
 etmap[wxEVT_NC_MIDDLE_UP] = new wxeEtype("nc_middle_up", 33);
 etmap[wxEVT_NC_RIGHT_DOWN] = new wxeEtype("nc_right_down", 33);
 etmap[wxEVT_NC_RIGHT_UP] = new wxeEtype("nc_right_up", 33);
 etmap[wxEVT_NC_MOTION] = new wxeEtype("nc_motion", 33);
 etmap[wxEVT_NC_ENTER_WINDOW] = new wxeEtype("nc_enter_window", 33);
 etmap[wxEVT_NC_LEAVE_WINDOW] = new wxeEtype("nc_leave_window", 33);
 etmap[wxEVT_NC_LEFT_DCLICK] = new wxeEtype("nc_left_dclick", 33);
 etmap[wxEVT_NC_MIDDLE_DCLICK] = new wxeEtype("nc_middle_dclick", 33);
 etmap[wxEVT_NC_RIGHT_DCLICK] = new wxeEtype("nc_right_dclick", 33);
 etmap[wxEVT_CREATE] = new wxeEtype("create", 34);
 etmap[wxEVT_COMMAND_BUTTON_CLICKED] = new wxeEtype("command_button_clicked", 35);
 etmap[wxEVT_COMMAND_CHECKBOX_CLICKED] = new wxeEtype("command_checkbox_clicked", 35);
 etmap[wxEVT_COMMAND_CHOICE_SELECTED] = new wxeEtype("command_choice_selected", 35);
 etmap[wxEVT_COMMAND_LISTBOX_SELECTED] = new wxeEtype("command_listbox_selected", 35);
 etmap[wxEVT_COMMAND_LISTBOX_DOUBLECLICKED] = new wxeEtype("command_listbox_doubleclicked", 35);
 etmap[wxEVT_COMMAND_TEXT_UPDATED] = new wxeEtype("command_text_updated", 35);
 etmap[wxEVT_COMMAND_TEXT_ENTER] = new wxeEtype("command_text_enter", 35);
 etmap[wxEVT_COMMAND_MENU_SELECTED] = new wxeEtype("command_menu_selected", 35);
 etmap[wxEVT_COMMAND_SLIDER_UPDATED] = new wxeEtype("command_slider_updated", 35);
 etmap[wxEVT_COMMAND_RADIOBOX_SELECTED] = new wxeEtype("command_radiobox_selected", 35);
 etmap[wxEVT_COMMAND_RADIOBUTTON_SELECTED] = new wxeEtype("command_radiobutton_selected", 35);
 etmap[wxEVT_COMMAND_SCROLLBAR_UPDATED] = new wxeEtype("command_scrollbar_updated", 35);
 etmap[wxEVT_COMMAND_VLBOX_SELECTED] = new wxeEtype("command_vlbox_selected", 35);
 etmap[wxEVT_COMMAND_COMBOBOX_SELECTED] = new wxeEtype("command_combobox_selected", 35);
 etmap[wxEVT_COMMAND_TOGGLEBUTTON_CLICKED] = new wxeEtype("command_togglebutton_clicked", 35);
 etmap[wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED] = new wxeEtype("command_notebook_page_changed", 35);
 etmap[wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGING] = new wxeEtype("command_notebook_page_changing", 35);
 etmap[wxEVT_JOY_BUTTON_DOWN] = new wxeEtype("joy_button_down", 36);
 etmap[wxEVT_JOY_BUTTON_UP] = new wxeEtype("joy_button_up", 36);
 etmap[wxEVT_JOY_MOVE] = new wxeEtype("joy_move", 36);
 etmap[wxEVT_JOY_ZMOVE] = new wxeEtype("joy_zmove", 36);
 etmap[wxEVT_QUERY_NEW_PALETTE] = new wxeEtype("query_new_palette", 37);
 etmap[wxEVT_MOVE] = new wxeEtype("move", 38);
 etmap[wxEVT_IDLE] = new wxeEtype("idle", 39);
 etmap[wxEVT_NC_PAINT] = new wxeEtype("nc_paint", 40);
 etmap[wxEVT_COMMAND_COLOURPICKER_CHANGED] = new wxeEtype("command_colourpicker_changed", 41);
 etmap[wxEVT_COMMAND_TREE_BEGIN_DRAG] = new wxeEtype("command_tree_begin_drag", 42);
 etmap[wxEVT_COMMAND_TREE_BEGIN_RDRAG] = new wxeEtype("command_tree_begin_rdrag", 42);
 etmap[wxEVT_COMMAND_TREE_BEGIN_LABEL_EDIT] = new wxeEtype("command_tree_begin_label_edit", 42);
 etmap[wxEVT_COMMAND_TREE_END_LABEL_EDIT] = new wxeEtype("command_tree_end_label_edit", 42);
 etmap[wxEVT_COMMAND_TREE_DELETE_ITEM] = new wxeEtype("command_tree_delete_item", 42);
 etmap[wxEVT_COMMAND_TREE_GET_INFO] = new wxeEtype("command_tree_get_info", 42);
 etmap[wxEVT_COMMAND_TREE_SET_INFO] = new wxeEtype("command_tree_set_info", 42);
 etmap[wxEVT_COMMAND_TREE_ITEM_EXPANDED] = new wxeEtype("command_tree_item_expanded", 42);
 etmap[wxEVT_COMMAND_TREE_ITEM_EXPANDING] = new wxeEtype("command_tree_item_expanding", 42);
 etmap[wxEVT_COMMAND_TREE_ITEM_COLLAPSED] = new wxeEtype("command_tree_item_collapsed", 42);
 etmap[wxEVT_COMMAND_TREE_ITEM_COLLAPSING] = new wxeEtype("command_tree_item_collapsing", 42);
 etmap[wxEVT_COMMAND_TREE_SEL_CHANGED] = new wxeEtype("command_tree_sel_changed", 42);
 etmap[wxEVT_COMMAND_TREE_SEL_CHANGING] = new wxeEtype("command_tree_sel_changing", 42);
 etmap[wxEVT_COMMAND_TREE_KEY_DOWN] = new wxeEtype("command_tree_key_down", 42);
 etmap[wxEVT_COMMAND_TREE_ITEM_ACTIVATED] = new wxeEtype("command_tree_item_activated", 42);
 etmap[wxEVT_COMMAND_TREE_ITEM_RIGHT_CLICK] = new wxeEtype("command_tree_item_right_click", 42);
 etmap[wxEVT_COMMAND_TREE_ITEM_MIDDLE_CLICK] = new wxeEtype("command_tree_item_middle_click", 42);
 etmap[wxEVT_COMMAND_TREE_END_DRAG] = new wxeEtype("command_tree_end_drag", 42);
 etmap[wxEVT_COMMAND_TREE_STATE_IMAGE_CLICK] = new wxeEtype("command_tree_state_image_click", 42);
 etmap[wxEVT_COMMAND_TREE_ITEM_GETTOOLTIP] = new wxeEtype("command_tree_item_gettooltip", 42);
 etmap[wxEVT_COMMAND_TREE_ITEM_MENU] = new wxeEtype("command_tree_item_menu", 42);
}

void wxeEvtListener::forward(wxEvent& event) 
{ 
#ifdef DEBUG
  if(!sendevent(&event, port)) 
    fprintf(stderr, "Couldn't send event!\r\n");
#else
sendevent(&event, port);
#endif
}

int getRef(void* ptr, wxeMemEnv* memenv) 
{ 
  WxeApp * app = (WxeApp *) wxTheApp;
  return app->getRef(ptr,memenv);
} 

bool sendevent(wxEvent *event, ErlDrvPort port)
{
 int send_res ;
 char * evClass = NULL;
 wxMBConvUTF32 UTFconverter;
 wxeEtype *Etype = etmap[event->GetEventType()];
 wxeCallbackData *cb = (wxeCallbackData *)event->m_callbackUserData;
  WxeApp * app = (WxeApp *) wxTheApp;
 wxeMemEnv *memenv = app->getMemEnv(port);
  wxeReturn rt = wxeReturn(port, cb->listener);

 rt.addAtom((char*)"wx");
 rt.addInt((int) event->GetId());
 rt.addRef(getRef((void *)(cb->obj), memenv), cb->class_name);
 rt.addExt2Term(cb->user_data);
 if(!memenv) return 0;

 switch(Etype->cID) {
case 1: {// wxNavigationKeyEvent
 wxNavigationKeyEvent * ev = (wxNavigationKeyEvent *) event;
  evClass = (char*)"wxNavigationKeyEvent";
  rt.addAtom((char*)"wxNavigationKey");
  rt.addAtom(Etype->eName);
 rt.addInt(ev->m_flags);
 rt.addRef(getRef((void *)ev->m_focus,memenv), "wxWindow");
  rt.addTupleCount(4);
  break;
}
case 2: {// wxSashEvent
 wxSashEvent * ev = (wxSashEvent *) event;
  evClass = (char*)"wxSashEvent";
  rt.addAtom((char*)"wxSash");
  rt.addAtom(Etype->eName);
 rt.addInt(ev->GetEdge());
 rt.add(ev->GetDragRect());
 rt.addInt(ev->GetDragStatus());
  rt.addTupleCount(5);
  break;
}
case 3: {// wxListEvent
 wxListEvent * ev = (wxListEvent *) event;
  evClass = (char*)"wxListEvent";
  rt.addAtom((char*)"wxList");
  rt.addAtom(Etype->eName);
 rt.addInt(ev->GetKeyCode());
 rt.addInt(ev->m_oldItemIndex);
 rt.addInt(ev->GetIndex());
 rt.addInt(ev->m_col);
 rt.add(ev->GetPoint());
  rt.addTupleCount(7);
  break;
}
case 4: {// wxNotebookEvent
  evClass = (char*)"wxNotebookEvent";
  rt.addAtom((char*)"wxNotebook");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 5: {// wxDisplayChangedEvent
  evClass = (char*)"wxDisplayChangedEvent";
  rt.addAtom((char*)"wxDisplayChanged");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 6: {// wxEraseEvent
 wxEraseEvent * ev = (wxEraseEvent *) event;
 wxDC * GetDC = ev->GetDC();
  evClass = (char*)"wxEraseEvent";
  rt.addAtom((char*)"wxErase");
  rt.addAtom(Etype->eName);
 rt.addRef(getRef((void *)GetDC,memenv), "wxDC");
  rt.addTupleCount(3);
  break;
}
case 7: {// wxKeyEvent
 wxKeyEvent * ev = (wxKeyEvent *) event;
  evClass = (char*)"wxKeyEvent";
  rt.addAtom((char*)"wxKey");
  rt.addAtom(Etype->eName);
 rt.addInt(ev->m_x);
 rt.addInt(ev->m_y);
 rt.addInt(ev->m_keyCode);
 rt.addBool(ev->m_controlDown);
 rt.addBool(ev->m_shiftDown);
 rt.addBool(ev->m_altDown);
 rt.addBool(ev->m_metaDown);
 rt.addBool(ev->m_scanCode);
 rt.addInt(ev->m_uniChar);
 rt.addUint(ev->m_rawCode);
 rt.addUint(ev->m_rawFlags);
  rt.addTupleCount(13);
  break;
}
case 8: {// wxWindowDestroyEvent
  evClass = (char*)"wxWindowDestroyEvent";
  rt.addAtom((char*)"wxWindowDestroy");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 9: {// wxCalendarEvent
  evClass = (char*)"wxCalendarEvent";
  rt.addAtom((char*)"wxCalendar");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 10: {// wxScrollEvent
  evClass = (char*)"wxScrollEvent";
  rt.addAtom((char*)"wxScroll");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 11: {// wxMenuEvent
  evClass = (char*)"wxMenuEvent";
  rt.addAtom((char*)"wxMenu");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 12: {// wxContextMenuEvent
  evClass = (char*)"wxContextMenuEvent";
  rt.addAtom((char*)"wxContextMenu");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 13: {// wxShowEvent
  evClass = (char*)"wxShowEvent";
  rt.addAtom((char*)"wxShow");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 14: {// wxSetCursorEvent
  evClass = (char*)"wxSetCursorEvent";
  rt.addAtom((char*)"wxSetCursor");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 15: {// wxFontPickerEvent
 wxFontPickerEvent * ev = (wxFontPickerEvent *) event;
 wxFont * GetFont = new wxFont(ev->GetFont());
 app->newPtr((void *) GetFont,3, memenv);
  evClass = (char*)"wxFontPickerEvent";
  rt.addAtom((char*)"wxFontPicker");
  rt.addAtom(Etype->eName);
 rt.addRef(getRef((void *)GetFont,memenv), "wxFont");
  rt.addTupleCount(3);
  break;
}
case 16: {// wxScrollWinEvent
  evClass = (char*)"wxScrollWinEvent";
  rt.addAtom((char*)"wxScrollWin");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 17: {// wxPaintEvent
  evClass = (char*)"wxPaintEvent";
  rt.addAtom((char*)"wxPaint");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 18: {// wxChildFocusEvent
  evClass = (char*)"wxChildFocusEvent";
  rt.addAtom((char*)"wxChildFocus");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 19: {// wxMaximizeEvent
  evClass = (char*)"wxMaximizeEvent";
  rt.addAtom((char*)"wxMaximize");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 20: {// wxFileDirPickerEvent
 wxFileDirPickerEvent * ev = (wxFileDirPickerEvent *) event;
  evClass = (char*)"wxFileDirPickerEvent";
  rt.addAtom((char*)"wxFileDirPicker");
  rt.addAtom(Etype->eName);
 rt.add(ev->GetPath());
  rt.addTupleCount(3);
  break;
}
case 21: {// wxFocusEvent
  evClass = (char*)"wxFocusEvent";
  rt.addAtom((char*)"wxFocus");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 22: {// wxDateEvent
  evClass = (char*)"wxDateEvent";
  rt.addAtom((char*)"wxDate");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 23: {// wxHelpEvent
  evClass = (char*)"wxHelpEvent";
  rt.addAtom((char*)"wxHelp");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 24: {// wxStyledTextEvent
 wxStyledTextEvent * ev = (wxStyledTextEvent *) event;
  evClass = (char*)"wxStyledTextEvent";
  rt.addAtom((char*)"wxStyledText");
  rt.addAtom(Etype->eName);
 rt.addInt(ev->GetPosition());
 rt.addInt(ev->GetKey());
 rt.addInt(ev->GetModifiers());
 rt.addInt(ev->GetModificationType());
 rt.add(ev->GetText());
 rt.addInt(ev->GetLength());
 rt.addInt(ev->GetLinesAdded());
 rt.addInt(ev->GetLine());
 rt.addInt(ev->GetFoldLevelNow());
 rt.addInt(ev->GetFoldLevelPrev());
 rt.addInt(ev->GetMargin());
 rt.addInt(ev->GetMessage());
 rt.addInt(ev->GetWParam());
 rt.addInt(ev->GetLParam());
 rt.addInt(ev->GetListType());
 rt.addInt(ev->GetX());
 rt.addInt(ev->GetY());
 rt.add(ev->GetDragText());
 rt.addBool(ev->GetDragAllowMove());
 rt.addInt(ev->GetDragResult());
  rt.addTupleCount(22);
  break;
}
case 25: {// wxSysColourChangedEvent
  evClass = (char*)"wxSysColourChangedEvent";
  rt.addAtom((char*)"wxSysColourChanged");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 26: {// wxGridEvent
 wxGridEvent * ev = (wxGridEvent *) event;
  evClass = (char*)"wxGridEvent";
  rt.addAtom((char*)"wxGrid");
  rt.addAtom(Etype->eName);
 rt.addInt(ev->GetRow());
 rt.addInt(ev->GetCol());
 rt.addInt(ev->GetPosition().x);
 rt.addInt(ev->GetPosition().y);
 rt.addBool(ev->Selecting());
 rt.addBool(ev->ControlDown());
 rt.addBool(ev->MetaDown());
 rt.addBool(ev->ShiftDown());
 rt.addBool(ev->AltDown());
  rt.addTupleCount(11);
  break;
}
case 27: {// wxPaletteChangedEvent
  evClass = (char*)"wxPaletteChangedEvent";
  rt.addAtom((char*)"wxPaletteChanged");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 28: {// wxUpdateUIEvent
  evClass = (char*)"wxUpdateUIEvent";
  rt.addAtom((char*)"wxUpdateUI");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 29: {// wxSizeEvent
 wxSizeEvent * ev = (wxSizeEvent *) event;
  evClass = (char*)"wxSizeEvent";
  rt.addAtom((char*)"wxSize");
  rt.addAtom(Etype->eName);
 rt.add(ev->m_size);
 rt.add(ev->m_rect);
  rt.addTupleCount(4);
  break;
}
case 30: {// wxIconizeEvent
  evClass = (char*)"wxIconizeEvent";
  rt.addAtom((char*)"wxIconize");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 31: {// wxCloseEvent
  evClass = (char*)"wxCloseEvent";
  rt.addAtom((char*)"wxClose");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 32: {// wxMouseCaptureChangedEvent
  evClass = (char*)"wxMouseCaptureChangedEvent";
  rt.addAtom((char*)"wxMouseCaptureChanged");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 33: {// wxMouseEvent
 wxMouseEvent * ev = (wxMouseEvent *) event;
  evClass = (char*)"wxMouseEvent";
  rt.addAtom((char*)"wxMouse");
  rt.addAtom(Etype->eName);
 rt.addInt(ev->m_x);
 rt.addInt(ev->m_y);
 rt.addBool(ev->m_leftDown);
 rt.addBool(ev->m_middleDown);
 rt.addBool(ev->m_rightDown);
 rt.addBool(ev->m_controlDown);
 rt.addBool(ev->m_shiftDown);
 rt.addBool(ev->m_altDown);
 rt.addBool(ev->m_metaDown);
 rt.addInt(ev->m_wheelRotation);
 rt.addInt(ev->m_wheelDelta);
 rt.addInt(ev->m_linesPerAction);
  rt.addTupleCount(14);
  break;
}
case 34: {// wxWindowCreateEvent
  evClass = (char*)"wxWindowCreateEvent";
  rt.addAtom((char*)"wxWindowCreate");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 35: {// wxCommandEvent
 wxCommandEvent * ev = (wxCommandEvent *) event;
  evClass = (char*)"wxCommandEvent";
  rt.addAtom((char*)"wxCommand");
  rt.addAtom(Etype->eName);
 rt.add(ev->GetString());
 rt.addInt(ev->GetInt());
 rt.addInt(ev->GetExtraLong());
  rt.addTupleCount(5);
  break;
}
case 36: {// wxJoystickEvent
  evClass = (char*)"wxJoystickEvent";
  rt.addAtom((char*)"wxJoystick");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 37: {// wxQueryNewPaletteEvent
  evClass = (char*)"wxQueryNewPaletteEvent";
  rt.addAtom((char*)"wxQueryNewPalette");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 38: {// wxMoveEvent
  evClass = (char*)"wxMoveEvent";
  rt.addAtom((char*)"wxMove");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 39: {// wxIdleEvent
  evClass = (char*)"wxIdleEvent";
  rt.addAtom((char*)"wxIdle");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 40: {// wxNcPaintEvent
  evClass = (char*)"wxNcPaintEvent";
  rt.addAtom((char*)"wxNcPaint");
  rt.addAtom(Etype->eName);
  rt.addTupleCount(2);
  break;
}
case 41: {// wxColourPickerEvent
 wxColourPickerEvent * ev = (wxColourPickerEvent *) event;
  evClass = (char*)"wxColourPickerEvent";
  rt.addAtom((char*)"wxColourPicker");
  rt.addAtom(Etype->eName);
 rt.add(ev->GetColour());
  rt.addTupleCount(3);
  break;
}
case 42: {// wxTreeEvent
 wxTreeEvent * ev = (wxTreeEvent *) event;
  evClass = (char*)"wxTreeEvent";
  rt.addAtom((char*)"wxTree");
  rt.addAtom(Etype->eName);
 rt.addRef(getRef((void *)ev->GetItem().m_pItem,memenv), "wxTreeItemId");
 rt.addRef(getRef((void *)ev->GetOldItem().m_pItem,memenv), "wxTreeItemId");
  rt.addTupleCount(4);
  break;
}
 }

 rt.addTupleCount(5);
 if(cb->fun_id) {
   rt.addRef(getRef((void *)event,memenv), evClass);
   rt.addTupleCount(2);
   rt.addInt(cb->fun_id);
   rt.addAtom("_wx_invoke_cb_");
   rt.addTupleCount(3);
   pre_callback();
   send_res =  rt.send();
   if(send_res) handle_event_callback(port, cb->listener);
   app->clearPtr((void *) event);
 } else {
   send_res =  rt.send();
   if(cb->skip) event->Skip();
 };
 return send_res;
 }
