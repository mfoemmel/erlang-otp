#-*-makefile-*-   ; force emacs to enter makefile-mode


SUB_DIRECTORIES = simple interop 
ifeq ($(BUILD_SIP),true)
  SUB_DIRECTORIES += sip_isdn_gw
endif

