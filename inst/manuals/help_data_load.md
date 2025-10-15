## 🗂️ Data Load Panel User Manual

### 📝 Overview

The `data_load_panel()` Shiny module provides an interface for loading datasets and corresponding TIFF image frames for analysis.  
It allows users to select predefined folders within the Fly Survival App directory structure or browse anywhere on the computer to import frames and analysis files.

This panel is usually the **first step** in the Fly Survival App workflow, preparing data for downstream modules such as browsing, inference, and export.

---

### 🧩 UI Layout

The user interface is a Shiny **tab panel** composed of:

- A **sidebar** for selecting and loading data.  
- A **main panel** displaying instructions and feedback messages.

---

### 🔹 Sidebar Components

| Control | Type | Purpose |
|----------|------|----------|
| **Browse anywhere on computer** (`custom_paths`) | Checkbox | Enables the selection of folders and files from any location on the computer. |
| **Frame Subfolder** (`frame_folder_ui`) | Select Input | Selects a subfolder under `avi_frames` containing TIFF frames (shown when `custom_paths = FALSE`). |
| **Analysis File** (`stats_file_ui`) | Select Input | Chooses a `.txt` analysis file from `flySurvivalApp_output` (shown when `custom_paths = FALSE`). |
| **Choose Image Folder** (`custom_frame_dir`) | `shinyDirButton` | Selects any folder on the computer containing `.tif` frames (shown when `custom_paths = TRUE`). |
| **Choose Stats File** (`custom_stats_file`) | `shinyFilesButton` | Selects any `.txt` analysis file on the computer (shown when `custom_paths = TRUE`). |
| **Load / Reload Dataset** (`reload_stats_btn`) | Action Button | Loads or reloads the selected dataset and frame files. |

---

### 🔸 Main Panel Components

| Output | Type | Description |
|---------|------|-------------|
| **Instruction Text** | HTML / Text | Explains how to choose frame subfolders and analysis files, or use the browse-anywhere option. |

---

### 🔄 Workflow

#### **Step 1: Select Data Source**

By default, internal folders are used:

- **Frames:** `avi_frames/<subfolder>`  
- **Analysis:** `flySurvivalApp_output/<file>.txt`

If **“Browse anywhere on computer”** is checked:

- Use the folder picker to select a directory containing `.tif` frames.  
- Use the file picker to select the corresponding `.txt` analysis file.

---

#### **Step 2: Load Dataset and Frames**

Click **“Load / Reload Dataset”** to initiate loading.

The module performs the following actions:

1. Reads all `.tif` frames from the selected folder.  
2. Sorts frames **numerically** based on frame numbers in filenames.  
3. Loads the selected analysis `.txt` file into `df_analysis`.  
4. Updates `frame_paths` with the full paths of loaded frames.  
5. Displays a Shiny notification:  
   > ✅ *Dataset loaded successfully.*

---

#### **Step 3: Data Validation**

The module requires:

- **Frames:** TIFF files (`.tif`) with filenames containing frame numbers.  
- **Analysis file:** Tabular `.txt` file with bounding box data from inference.

⚠️ Any missing or incorrect paths will prevent dataset loading until corrected.

---

#### **Step 4: Downstream Usage**

Once data is successfully loaded, two **reactive elements** are created internally:

- `df_analysis()` → contains the dataset used in inference and browsing panels.  
- `frame_paths()` → contains the ordered list of image paths for browsing and plotting.

---

### ⚠️ Notes

| Feature | Behavior / Notes |
|----------|------------------|
| **Browsing internal folders** | Provides quick access to the standard app structure (`avi_frames` & `flySurvivalApp_output`). |
| **Browsing anywhere** | Requires `custom_paths = TRUE`. Opens `shinyFiles` dialogs to pick any folder or file. |
| **Reloading** | Clicking “Load / Reload Dataset” overwrites the current reactive containers. |
| **Frame order** | Automatically sorted numerically (e.g., `_001.tif`, `_002.tif`). |

---

### 🧠 Summary

The **Data Load Panel** ensures that the Fly Survival App has **consistent and correctly ordered image frames** and **associated analysis data** for all downstream processes.  
It offers flexibility to work with either the **default internal folder structure** or **custom directories** on the user’s computer.

🪰 *This module is the foundation of the Fly Survival App workflow, ensuring all subsequent analyses are accurate and reproducible.*
