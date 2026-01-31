#!/bin/bash

COMPILER="D:/F-100.1_no_apis/f-v23.6.0-Ryiuk_final_form_3.0/v14.0.0-Ryiuk_synthetic_evolution_1.0/ryiuk-core/tsc-rust/target/release/tsc-rust.exe"
FILES=$(find "/D/F-100.1_no_apis/clawd_rust/openclaw-main/openclaw-main" -name "*.ts" -type f | head -100)

TOTAL=0
SUCCESS=0

echo "Testing compiler on 100 TypeScript files..."
echo ""

for file in $FILES; do
    TOTAL=$((TOTAL + 1))
    OUTPUT=$("$COMPILER" "$file" 2>&1)
    EXIT_CODE=$?
    
    if [ $EXIT_CODE -eq 0 ]; then
        SUCCESS=$((SUCCESS + 1))
        echo "✓ $(basename "$file")"
    else
        # Extract error message
        ERROR_MSG=$(echo "$OUTPUT" | grep -o "Error:.*" | head -1 | sed 's/Error://' | sed 's/^ *//')
        if [ -n "$ERROR_MSG" ]; then
            echo "✗ $(basename "$file") - $ERROR_MSG"
        else
            echo "✗ $(basename "$file") - Unknown error"
        fi
    fi
done

echo ""
echo "========================================"
echo "RESULTS: $SUCCESS / $TOTAL"
PERCENTAGE=$(awk "BEGIN {printf \"%.1f\", ($SUCCESS/$TOTAL)*100}")
echo "Success Rate: $PERCENTAGE%"
echo "========================================"
