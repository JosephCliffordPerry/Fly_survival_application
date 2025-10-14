# 🧭 Browse Panel User Manual

## 📝 Overview

The `browse_panel()` **Shiny module** provides an interactive interface for browsing, analyzing, and propagating object detections across sequential image frames (TIFF images extracted from videos).  
It is part of the **FlySurvival App pipeline** and is designed to **visualize, merge, and edit inference results** derived from previous analysis steps.

📥 Arguments
Argument	Type	Description
df_analysis	reactive data frame	Reactive container holding inference results (bounding boxes, frame numbers, etc.).
frame_paths	reactive character vector	Reactive list of TIFF frame paths to be browsed.
stats_file	character (optional)	Optional path to a saved analysis file for reloading prior results.

🧩 UI Layout
The UI is structured as a Shiny tab panel with a sidebar and a main content area.

🔹 Sidebar Components
Control	Type	Purpose
IoU Threshold (iou_threshold)	Numeric Input	Sets the Intersection-over-Union (IoU) threshold for merging overlapping boxes (default: 0.3).
Minimum Appearances per ID (min_appearances)	Numeric Input	Filters out object IDs appearing fewer than this number of times (default: 2). 
Show Only Propagated Boxes (show_only_propagated)	Checkbox	Toggles visibility of boxes that were algorithmically propagated.
Load/Analyse Input File (run_analysis)	Action Button	Loads or re-analyzes the current inference data.
Save Analysis File (save_analysis)	Action Button	Saves the current propagated dataset to a .txt file.
Remove ID (remove_id, remove_id_btn)	Numeric Input + Button	Removes a specific object ID from the current analysis.
Frame Slider (frame_slider_ui)	Dynamic UI	Slider for navigating between image frames.

🔸 Main Panel Components
Output	Type	Description
Frame Preview (frame_plot)	Plot Output	Displays the current frame with bounding boxes overlaid and color-coded by type.

🔄 Workflow
Step 1: Load Inference Data
Click “Load/Analyse input file” to load bounding box data from the provided df_analysis object.

The panel verifies required columns (x1…y4, frame) and initializes IDs if absent.

Step 2: Automatic Propagation
If no id column exists, the panel propagates boxes automatically:

Links boxes across frames using the IoU threshold.

Boxes below threshold receive new IDs.

Low-frequency detections (below min_appearances) are discarded.

Progress is shown with a Shiny progress bar.

Step 3: Manual Editing
Use the sidebar to:

Adjust IoU or minimum appearance thresholds.

Remove incorrect detections by entering an ID and clicking “Remove ID”.

Add new pupariation events by clicking on a frame (generates and propagates a new bounding box).

Step 4: Frame Browsing
Use the frame slider to scroll through TIFF images.
Bounding boxes are color-coded:

Color	Meaning
🟣 Purple	First appearance (manually added or initial frame)
🔴 Red	Original detection from inference
🔵 Blue	Forward propagated box
🟢 Green	Propagated after last frame (“pupa”)

Toggle “Show Only Propagated Boxes” to view only algorithmically generated boxes.

Step 5: Saving Results
Click “Save Analysis File” to write the propagated dataset to:

php-template
Copy code
flySurvivalApp_output/<base_name>_processed.txt
If the file exists, a numeric suffix (_1, _2, etc.) is appended automatically.

💾 Output Data Format
The saved .txt file includes the following columns:

Column	Description
frame	Frame filename
frame_num	Frame index (numeric)
id	Object identifier
x1…y4	Polygon corner coordinates
propagated	TRUE if box was propagated
prop_type	Propagation type (first, original, forward, pupa)
manual	TRUE if manually added

🖱️ Interactivity Notes
Click Events
Clicking inside a frame adds a new pupariation box centered at the click location.
The box is propagated to all subsequent frames automatically.

Notifications
Shiny notifications appear for:

✅ Successful file saves

❌ ID removal confirmations

⚠️ Errors during analysis

⚠️ Error Handling
Error Type	Trigger	Resolution
Missing columns	df_analysis lacks required fields	Ensure x1…y4 and frame columns are present.
Empty frame list	frame_paths is empty	Verify TIFF files are correctly loaded.
Invalid IoU	Malformed or self-intersecting polygons	Check sf installation and polygon coordinate validity.

🧠 Summary
The Browse Panel is designed to visualize, edit, and propagate object detections across sequential image frames.
It complements automated inference by enabling manual inspection, ID correction, and refinement of tracking results for export.
