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
logformat = '%(asctime)s - %(levelname)s - %(message)s'
# Configure the logging module
logging.basicConfig(filename=logfile, level=logging.INFO, format=logformat)

def get_logger(name: str) -> logging.Logger:
    """
    Returns a logger instance for the given name.
    This ensures consistent logging configuration across scripts.
    """
    return logging.getLogger(name)

def main():
    print(logfile)
    # logger = get_logger(__name__)
    # logging.basicConfig(filename='systemlog.txt', level=logging.INFO, format=logformat)
    # logging.info('Application started')
    # logging.warning('An error occurred')
    # logging.debug('This is a debug message')
    pass

if __name__ == "__main__":
    main()
