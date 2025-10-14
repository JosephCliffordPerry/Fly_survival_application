##🗂️ Data Load Panel User Manual
#📝 Overview

The data_load_panel() Shiny module provides an interface for loading datasets and corresponding TIFF image frames for analysis.
It allows users to select predefined folders within the FlySurvival App structure or browse anywhere on the computer to import frames and analysis files.

This panel is usually the first step in the FlySurvival analysis App workflow, preparing data for downstream modules like browsing an export.

#🧩 UI Layout

The UI is a Shiny tab panel with a sidebar for selecting data and a main panel with instructions.

#🔹 Sidebar Components
Control	Type	Purpose
Browse anywhere on computer (custom_paths)	Checkbox	Enables the selection of folders and files from any location on the computer.
Frame Subfolder (frame_folder_ui)	Select Input	Select a subfolder under avi_frames containing TIFF frames (shown when custom_paths = FALSE).
Analysis File (stats_file_ui)	Select Input	Choose a .txt analysis file from flySurvivalApp_output (shown when custom_paths = FALSE).
Choose Image Folder (custom_frame_dir)	shinyDirButton	Select any folder on the computer containing .tif frames (shown when custom_paths = TRUE).
Choose Stats File (custom_stats_file)	shinyFilesButton	Select any .txt analysis file on the computer (shown when custom_paths = TRUE).
Load / Reload Dataset (reload_stats_btn)	Action Button	Loads or reloads the selected dataset and frame files.
#🔸 Main Panel Components
Output	Type	Description
Instruction Text	HTML / Text	Explains how to choose frame subfolders and analysis files, or use the browse-anywhere option.
#🔄 Workflow
Step 1: Select Data Source

By default, internal folders are used:

Frames: avi_frames/<subfolder>

Analysis: flySurvivalApp_output/<file>.txt

If “Browse anywhere on computer” is ticked:

Use the folder picker to select a folder containing .tif frames.

Use the file picker to select the corresponding analysis .txt file.

Step 2: Load Dataset and Frames

Click “Load / Reload Dataset”.

The module performs the following:

Reads all .tif frames in the selected folder.

Sorts frames numerically based on the frame number in the filename.

Loads the selected analysis .txt file into df_analysis.

Updates frame_paths with the full paths to the loaded frames.

Displays a Shiny notification: “Dataset loaded”.

Step 3: Data Validation

The module requires:

Frames: TIFF files (.tif) with filenames containing frame numbers.

Analysis file: Tabular .txt file with bounding box data for inference.

Any missing or incorrect paths will prevent dataset loading until corrected.

Step 4: Downstream Usage

Once loaded two reactive elements in the program are internally created:

df_analysis() contains the dataset for use in inference or browse panels.

frame_paths() contains the ordered list of image paths for browsing and plotting.

⚠️ Notes
Feature	Behavior / Notes
Browsing internal folders	Quick access to standard dataset structure (avi_frames & flySurvivalApp_output).
Browsing anywhere	Requires custom_paths = TRUE. Use shinyFiles dialog to pick any folder/file.
Reloading	Clicking “Load / Reload Dataset” overwrites current reactive containers.
Frame order	Automatically sorted numerically by the number in the filename (e.g., _001.tif, _002.tif).
🧠 Summary

The Data Load Panel ensures the FlySurvival App has consistent and correctly ordered frames and associated analysis data for all subsequent processing steps.
It provides flexibility to work with internal folders or any external directory on the computer.
