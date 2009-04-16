
//  I have added this doxygen seems to get lost in some macros
//  in sashwin.h

enum wxSashDragStatus
   {
      wxSASH_STATUS_OK,
      wxSASH_STATUS_OUT_OF_RANGE
   };

// Macro needed in erlang
class WXDLLIMPEXP_XRC wxXmlResource : public wxObject
{
 public:
   wxObject xrcctrl(wxWindow *Window, wxString Name, wxString Type);
}
