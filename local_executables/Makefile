################################################################################
# Makefile for configuring scripts in local_executables
#

# Set variables
ARCHIVE_BASE_URL:="http://40.117.96.252/data/PurpleAir"

EXEC_DIR:=/home/jonathan/AirSensor/local_executables


################################################################################
# Create new crontab files and executable scripts
#
# NOTE:  Use "%" as the sed delimiter to avoid conflict with "/"

configure_crontabs:
	sed 's%__EXEC_DIR__%${EXEC_DIR}%g' __crontab_daily.txt > crontab_daily.txt
	sed 's%__EXEC_DIR__%${EXEC_DIR}%g' __crontab_archive.txt > crontab_archive.txt

configure_scripts:
	sed 's%__ARCHIVE_BASE_URL__%${ARCHIVE_BASE_URL}%g' __createAnnualAirSensor_exec.R > createAnnualAirSensor_exec.R
	sed 's%__ARCHIVE_BASE_URL__%${ARCHIVE_BASE_URL}%g' __createLatestAirSensor_exec.R > createLatestAirSensor_exec.R
	sed 's%__ARCHIVE_BASE_URL__%${ARCHIVE_BASE_URL}%g' __createLatestPAT_exec.R > createLatestPAT_exec.R
	sed 's%__ARCHIVE_BASE_URL__%${ARCHIVE_BASE_URL}%g' __createMonthlyAirSensor_exec.R > createMonthlyAirSensor_exec.R
	sed 's%__ARCHIVE_BASE_URL__%${ARCHIVE_BASE_URL}%g' __createMonthlyPAT_exec.R > createMonthlyPAT_exec.R
	sed 's%__ARCHIVE_BASE_URL__%${ARCHIVE_BASE_URL}%g' __createPAS_archival_exec.R > createPAS_archival_exec.R
	sed 's%__ARCHIVE_BASE_URL__%${ARCHIVE_BASE_URL}%g' __createPAS_exec.R > createPAS_exec.R
	sed 's%__ARCHIVE_BASE_URL__%${ARCHIVE_BASE_URL}%g' __createVideo_exec.R > createVideo_exec.R
	chmod +x createAnnualAirSensor_exec.R
	chmod +x createLatestAirSensor_exec.R
	chmod +x createLatestPAT_exec.R
	chmod +x createMonthlyAirSensor_exec.R
	chmod +x createMonthlyPAT_exec.R
	chmod +x createPAS_archival_exec.R
	chmod +x createPAS_exec.R
	chmod +x createVideo_exec.R

configure: configure_crontabs configure_scripts
	
