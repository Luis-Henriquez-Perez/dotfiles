# Filename: update_email.py
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-17 20:02:52
# Description: Pull email from the cloud and index it.

import subprocess
from log import get_logger

logger = get_logger(__name__)

def run_mu_index():
    try:
        # Run "mu index" and capture its output
        result = subprocess.run(
            ["mu", "index"],  # Command and arguments as a list
            check=True,       # Raise an exception if the command fails
            capture_output=True,  # Capture stdout and stderr
            text=True         # Decode output as text (str) rather than bytes
        )
        print("Output:", result.stdout)  # Print the output of the command
    except subprocess.CalledProcessError as e:
        # Handle command errors
        print("Error:", e.stderr)
        print("Return Code:", e.returncode)
# mbsync -a
# mu index
# notmuch new
logger.info("")
logger.info("")
