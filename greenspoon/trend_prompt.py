import anthropic
import csv
import pandas as pd

client = anthropic.Anthropic()

# Your system prompt
SYSTEM_PROMPT = """
[Your entire prompt goes here - the one you showed me]
"""

def process_batch(rows, batch_size=10):
    """Process rows in batches"""
    results = []
    wild_cards = []
    
    for i in range(0, len(rows), batch_size):
        batch = rows[i:i+batch_size]
        
        # Create message with batch data
        user_message = f"""
        Process these {len(batch)} rows of conservation data:
        
        {format_batch_for_claude(batch)}
        
        Output two CSV sections:
        1. PROCESSED_DATA: Clean processed rows
        2. WILD_CARDS: Rows too confusing to parse
        """
        
        response = client.messages.create(
            model="claude-3-5-sonnet-20241022",
            max_tokens=8000,
            system=SYSTEM_PROMPT,
            messages=[{"role": "user", "content": user_message}]
        )
        
        # Parse response and add to results
        processed, wild = parse_claude_response(response.content[0].text)
        results.extend(processed)
        wild_cards.extend(wild)
        
        print(f"Processed batch {i//batch_size + 1}")
    
    return results, wild_cards


# converts the csv passed in to something like
# csv_lines = [
    #   'Red List status,Population trend,Year assessed',
    #   'LC,stable,2017', 
    #   'EN,decreasing,2008'
    # ]
# but with \n where the new lines are
def format_batch_for_claude(batch):
    """Convert batch to CSV format for Claude"""
    if not batch:
        return ""
    
    # Get column headers from first row
    headers = list(batch[0].keys())
    
    # Create CSV string
    csv_lines = [','.join(headers)]
    for row in batch:
        csv_lines.append(','.join(str(row.get(col, '')) for col in headers))
    
    return '\n'.join(csv_lines)
    
# Take claude's response and convert it to two csv files
def parse_claude_response(response_text):
    """Parse Claude's CSV response into processed and wild cards"""
    try:
        # Split response into sections
        parts = response_text.split('WILD_CARDS:')
        processed_part = parts[0].replace('PROCESSED_DATA:', '').strip()
        wild_part = parts[1].strip() if len(parts) > 1 else ""
        
        # Convert CSV text back to dictionaries
        processed = csv_text_to_dicts(processed_part)
        wild_cards = csv_text_to_dicts(wild_part) if wild_part else []
        
        return processed, wild_cards
    except:
        # If parsing fails, treat as wild card
        return [], [{"raw_response": response_text}]

#converts csv files into dictionaries
# Dictionaries let you work with named columns instead of just positions.
# The data journey: Your original CSV → Python dictionaries → Claude processes 
# → Claude returns CSV text → Back to Python dictionaries → Final CSV file
def csv_text_to_dicts(csv_text):
    """Convert CSV text to list of dictionaries"""
    if not csv_text.strip():
        return []
    
    lines = csv_text.strip().split('\n')
    if len(lines) < 2:
        return []
    
    headers = [h.strip() for h in lines[0].split(',')]
    rows = []
    for line in lines[1:]:
        values = [v.strip() for v in line.split(',')]
        rows.append(dict(zip(headers, values)))
    
    return rows

# Main execution
if __name__ == "__main__": #this line means that this only runs if the file is run directly (i.e not through import)
    # Load your data
    df = pd.read_csv("small_test_for_claude.csv")
    rows = df.to_dict('records')
    
    # Process in batches
    processed, wild_cards = process_batch(rows, batch_size=10)
    
    # Save results
    pd.DataFrame(processed).to_csv("processed_results.csv", index=False)
    pd.DataFrame(wild_cards).to_csv("wild_cards.csv", index=False)