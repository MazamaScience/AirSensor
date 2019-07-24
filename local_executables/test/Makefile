################################################################################
# Makefile for testing scripts against mazamascience/airsensor:latest
#

# A little Makefile magic from:  https://www.gnu.org/software/make/manual/html_node/File-Name-Functions.html#File-Name-Functions
BASE_PATH:=$(realpath ..)

# And a target to test it
show_base_path:
	echo $(BASE_PATH)

# Clean up generated files
clean:
	-rm -rf $(BASE_PATH)/output
	-rm -rf $(BASE_PATH)/logs
	

################################################################################
# Testing createVideo_exec.R

createVideo:
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createVideo_exec.R --outputDir=/app/output --logDir=/app/logs --communityID=SCSB



