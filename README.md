# Agora: Facilitating the discussion of qualitative coding with multiple coders


## How to use the output durnig the discussion

### General principles

* Each row captures how a code is changed
  * Things that are in the trash column needs to be rectified in the code files
* Each column capture what should end up being in individual's and agreed codebooks.
 * At the end of the discussion, the revised codebooks can be generated by copying each column, eliminate new lines, and sort it alphabetically.


### Allowed operations

* Moving a code vertically
* Adding a code to a new row
* Moving one's own code to the one's trash column
* Merge two vertically-consecutive cells
* Agreement
  * Moving a code from individual's column to the "Agreed" column
  * Moving previously agreed code to the *both* trash columns

### Scenarios
#### Both coders agree to create a new code that wasn't there before
Add the agreed code in the center column


#### Both coders agree to change the wording of a previously agreed code
Move the old code to the trash column for both coders

#### Both coders merge codes that they individually came up with to a new agreed code
* Create a new row for the new code
* Vertically move the old code from individual's column into the same row with the new code
* If multiple old codes are to be merged into one, create a merged cell in the agreed code to associate the agreed code with all of the old ones.
* Horizontally move the old code from individual's code column to individual's trash column

### One coder want to remove a code that wasn't agreed
Move the code to the trash column

### One coder want to edit the wording of one's own code without seeking agreement
* Move the code to the trash column
* On the same row, type a new wording
 
 
### After coding discussion

* To update code books (agreed or individual's), copy the a column from agora file into a blank text file in Sublime Text. Remove the header column. Sort lines. This content is the new code book. 

   * This step is taken care of by `agora_to_codebook.R`.

* To update the coded data, go through the trash column in the agora file and make corresponding changes

## Utilities for individual coders

* `coding_merge.R` merges the agreed codes and indiviudal's codes into one file for convenience in code lookup.
* `coding_split.R` extracts individual's code from the merged file (for git commit)


## TODO
* create a script to help discussion → extract an excerpt at a given timestamp (quote and codes from each person) to be able to paste in the Google Doc.
* create a script to read transcript files and generate code book. It should also separate agreed codebooks from user-defined codebook.
* create a script to automatically update the transcript files based on the downloaded agora Excel
* create a script to automatically update the code book based on the downloaded agora Excel
* change agora output to a formatted spreadsheet with column widths and wrapping settings
   * freeze pane on the first row
   * adjust the column widths (small trash columns)
   * make text justification (trash column should be cut-off)
   * remove line_no column



Low priority:
* anonymize parameters (currently has our names in it)
