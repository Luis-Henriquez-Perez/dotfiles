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
def run_command(command: list[str], description: str):
    """
    Run a shell command and log its result.

    Args:
        command (list[str]): The command to execute as a list of strings.
        description (str): A brief description of the command for logging purposes.

    Returns:
        None
    """
    try:
        # Execute the command
        result = subprocess.run(
            command,
            check=True,
            capture_output=True,
            text=True
        )
        logger.info(f"{description}: Success")
        logger.debug(f"Output: {result.stdout.strip()}")
    except subprocess.CalledProcessError as e:
        # Log failure details
        logger.error(f"{description}: Failed with return code {e.returncode}")
        logger.error(f"Command: {' '.join(command)}")
        logger.error(f"Error: {e.stderr.strip()}")

def update_email():
    """
    Synchronize, index, and update email.
    """
    logger.info("Starting email update process.")

    # Run mbsync -a
    run_command(["mbsync", "-a"], "pulling email")

    # Run mu index
    run_command(["mu", "index"], "Indexing emails with mu")

    # Run notmuch new
    run_command(["notmuch", "new"], "Updating notmuch database")

    logger.info("Email update process completed.")

if __name__ == "__main__":
    update_email()
