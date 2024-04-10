#!/bin/bash

URL=$QUTE_URL

FILENAME="$HOME/Downloads/$(date +%Y%m%d%H%M%S)-page.html"

monolith $URL > $FILENAME

echo "message-info 'Saved page to $FILENAME'" >> $QUTE_FIFO
