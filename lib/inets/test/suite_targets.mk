#-*-makefile-*-   ; force emacs to enter makefile-mode
# ----------------------------------------------------
# Make include file for otp
#
# Copyright (C) 1996, Ericsson Telecommunications
# Author: Micael Karlberg
# ----------------------------------------------------

test: $(TARGET_FILES)
	erl -noshell -sname inets_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(INETS_SUITE) all \
            -s erlang halt

app: $(TARGET_FILES)
	erl -noshell -sname app_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(APP_SUITE) all \
            -s erlang halt

appup: $(TARGET_FILES)
	erl -noshell -sname appup_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(APPUP_SUITE) all \
            -s erlang halt

ftp: $(TARGET_FILES)
	erl -noshell -sname ftp_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(FTP_SUITE) all \
            -s erlang halt

ftp_cover: $(TARGET_FILES)
	erl -noshell -sname ftp_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_lib cover $(FTP_SUITE) all \
            -s erlang halt

httpd: $(TARGET_FILES)
	erl -noshell -sname httpd_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) all \
            -s erlang halt

httpd_cover: $(TARGET_FILES)
	erl -noshell -sname httpd_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_lib cover $(HTTPD_SUITE) all \
            -s erlang halt

ip: $(TARGET_FILES)
	erl -noshell -sname httpd_ip_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ip_comm \
            -s erlang halt

ip_cover: $(TARGET_FILES)
	erl -noshell -sname httpd_ip_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_lib cover $(HTTPD_SUITE) ip_comm \
            -s erlang halt

ip_mod: $(TARGET_FILES)
	erl -noshell -sname httpd_ip_mod_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ip_mod \
            -s erlang halt

ip_load: $(TARGET_FILES)
	erl -noshell -sname httpd_ip_load_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ip_load \
            -s erlang halt

ip_misc: $(TARGET_FILES)
	erl -noshell -sname httpd_ip_misc_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ip_misc \
            -s erlang halt

ip_block: $(TARGET_FILES)
	erl -noshell -sname httpd_ip_block_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ip_block \
            -s erlang halt

ip_restart: $(TARGET_FILES)
	erl -noshell -sname httpd_ip_restart_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ip_restart \
            -s erlang halt

ssl: $(TARGET_FILES)
	erl -noshell -sname httpd_ssl_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ssl \
            -s erlang halt


ssl_mod: $(TARGET_FILES)
	erl -noshell -sname httpd_ssl_misc_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ssl_mod \
            -s erlang halt

ssl_load: $(TARGET_FILES)
	erl -noshell -sname httpd_ssl_load_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ssl_load \
            -s erlang halt

ssl_misc: $(TARGET_FILES)
	erl -noshell -sname httpd_ssl_misc_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ssl_misc \
            -s erlang halt

ssl_block: $(TARGET_FILES)
	erl -noshell -sname httpd_ssl_block_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ssl_block \
            -s erlang halt

ssl_restart: $(TARGET_FILES)
	erl -noshell -sname httpd_ssl_restart_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPD_SUITE) ssl_restart \
            -s erlang halt

httpc: $(TARGET_FILES)
	erl -noshell -sname httpc_tester -pa $(INETS_ROOT)/ebin \
            -s inets_test_server t $(HTTPC_SUITE) all \
            -s erlang halt

