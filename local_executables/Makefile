################################################################################
# Makefile for configuring scripts in local_executables
#

hell whoami A little Makefile magic from:  https://www.gnu.org/software/make/manual/html_node/Flavors.html#Flavors
USER_NAME:=$(shell whoami)

# And a target to test it
show_user_name:
	echo $(USER_NAME)

################################################################################
# PAS objects

configure:
	sed 's/__USER_NAME__/${USER_NAME}/g' __crontab_daily.txt > crontab_daily.txt

