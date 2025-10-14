# 🖼️ Individual Pupa Browser (Largest Box Panel) User Manual

## 📝 Overview

The `largest_box_panel()` **Shiny module** provides an interface to inspect the **largest oriented bounding box (OBB) per object ID** across sequential image frames (TIFF images).  
It allows users to select an object ID and visualize its largest detection, optionally cropping the frame to that box.  
Users can also **move the first appearance** of the object to a different frame if needed.

This panel is typically used **after inference and browse panels** to focus on individual objects for inspection or reporting.

---

## ⚙️ Function Signature

largest_box_panel(df_analysis, frame_paths, stats_file = NULL)
📥 Arguments
Argument	Type	Description
df_analysis	reactive data frame	Reactive container holding propagated inference results with id, bounding box coordinates, frame numbers, and propagation type.
frame_paths	reactive character vector	Reactive container listing paths to TIFF frames.
stats_file	character (optional)	Optional path to a saved analysis file for reloading prior results.

🧩 UI Layout
The UI is a Shiny tab panel with a sidebar for selecting object IDs and a main panel for plotting the largest bounding box.

🔹 Sidebar Components
Control	Type	Purpose
ID Selector (id_selector_ui)	Select Input	Choose an object ID to display its largest OBB.
Move First Appearance (move_first_btn)	Action Button	Move the first appearance of the selected ID to the currently displayed frame.

🔸 Main Panel Components
Output	Type	Description
Largest Box Crop (largest_box_plot)	Plot Output	Displays the selected frame cropped to the largest OBB of the chosen ID, with bounding box overlaid.
Frame Slider (frame_slider_ui2)	Slider Input	Navigate through the sequence of frames.

🔄 Workflow
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

⚠️ Notes
Feature	Behavior / Notes
ID selection	Populated dynamically based on largest OBB per ID.
Cropping	Crops frame to bounding box extent for visualization; does not modify the original image.
First appearance	Can be moved to any frame using the sidebar button; updates df_analysis.
Automatic slider update	Slider auto-updates to the first appearance frame when ID is changed.

🧠 Summary
The Largest Box Panel provides an efficient way to inspect key detections per object across sequential frames.
It complements the Browse Panel by focusing on individual objects and their largest bounding boxes, and allows adjustments to first appearance for downstream analysis or reporting.
