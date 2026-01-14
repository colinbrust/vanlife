#!/bin/bash

LOCATIONS_FILE="locs.json"
COORDS_FILE="locs-coords.json"

# Create coords file if it doesn't exist
if [ ! -f "$COORDS_FILE" ]; then
    echo "[]" > "$COORDS_FILE"
fi

# Process each location
while IFS= read -r location; do
    LOC_NAME=$(echo "$location" | jq -r '.location')
    DATE_START=$(echo "$location" | jq -r '.date_start')
    
    # Check if already exists in coords file
    EXISTS=$(jq --arg name "$LOC_NAME" --arg date "$DATE_START" \
        'any(.[]; .location == $name and .date_start == $date)' "$COORDS_FILE")
    
    if [ "$EXISTS" = "false" ]; then
        echo "Geocoding: $LOC_NAME"
        
        # URL encode the location name
        ENCODED=$(echo "$LOC_NAME" | jq -sRr @uri)
        
        # Hit Nominatim API
        RESPONSE=$(curl -s "https://nominatim.openstreetmap.org/search?format=json&q=$ENCODED")
        
        # Extract coordinates
        LAT=$(echo "$RESPONSE" | jq -r '.[0].lat // 0')
        LNG=$(echo "$RESPONSE" | jq -r '.[0].lon // 0')
        
        # Add to coords file
        jq --arg name "$LOC_NAME" --arg date "$DATE_START" \
            --arg lat "$LAT" --arg lng "$LNG" \
            '. += [{location: $name, date_start: $date, lat: ($lat | tonumber), lng: ($lng | tonumber)}]' \
            "$COORDS_FILE" > "$COORDS_FILE.tmp" && mv "$COORDS_FILE.tmp" "$COORDS_FILE"
        
        # Respect Nominatim rate limit (1 request per second)
        sleep 1
    fi
done < <(jq -c '.[]' "$LOCATIONS_FILE")

echo "Geocoding complete. Results saved to $COORDS_FILE"