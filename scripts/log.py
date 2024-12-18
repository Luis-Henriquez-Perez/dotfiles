# Filename: log.py
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-17 18:25:06
# Description: This file is meant to provide logging setup to other scripts.
# The idea is to have a common log for all of my scripts so that I can tell
# whether the right thing is happening or not.  Each script will log to the same
# file via this logger.

import logging
import os

logfile = os.path.abspath(os.path.expanduser("~/.cache/systemlog.txt"))
logformat = '%(asctime)s - %(levelname)s - [%(filename)s] - %(message)s'
# Configure the logging module
logging.basicConfig(filename=logfile, level=logging.INFO, format=logformat)

def get_logger(name: str) -> logging.Logger:
    """
    Returns a logger instance for the given name.
    This ensures consistent logging configuration across scripts.
    """
    return logging.getLogger(name)

if __name__ == "__main__":

    main()
