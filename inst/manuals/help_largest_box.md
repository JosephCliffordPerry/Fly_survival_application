# Individual Pupa Browser (Largest Box Panel) User Manual

##  Overview

Workflow
Step 1: Select Object ID
The ID selector dropdown is automatically populated with IDs having largest OBBs.

If no ID is selected, the first available ID is used.

Step 2: Browse Frames
Use the frame slider to move through the sequence of frames.

The main panel displays:

Cropped image of the frame containing the largest OBB.

Bounding box polygon:

Color	Meaning
Purple	First appearance (prop_type == 'first')
Red	Other frames (prop_type != 'first')

Step 3: Move First Appearance
Select an ID.

Move the first appearance to the current frame by clicking “Move First Appearance to Current Frame”.

The prop_type column in df_analysis is updated:

Previous first appearance → "original"

New frame → "first"

A Shiny notification confirms the change.

Step 4: Visualization Notes
The largest OBB is determined per ID by calculating polygon areas for all frames.

Frames are displayed with cropping around the bounding box for focused inspection.

The slider updates automatically to the first appearance if available.


ID selection	Populated dynamically based on largest OBB per ID.
Cropping	Crops frame to bounding box extent for visualization; does not modify the original image.
First appearance	Can be moved to any frame using the sidebar button; updates df_analysis.
Automatic slider update	Slider auto-updates to the first appearance frame when ID is changed.

Summary
The Largest Box Panel provides an efficient way to inspect key detections per object across sequential frames.
It complements the Browse Panel by focusing on individual objects and their largest bounding boxes, and allows adjustments to first appearance for downstream analysis or reporting.
