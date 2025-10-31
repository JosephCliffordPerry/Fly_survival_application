# 🪰 General Pipeline Overview

This software processes videos recorded in terrariums and generates an Excel file containing every detected **pupariation event**.  
It combines automated neural network inference with manual validation tools to ensure accurate detection and timing.

---

## Pipeline Steps

### **1. Convert Video to Frames**

Use the **AVI → TIFF Converter** to transform your recorded `.avi` video into a sequence of individual image frames.  
These frames serve as the foundation for all subsequent analysis steps.

---

### **2. Run Neural Network Inference**

In the **Inference** tab, run the neural network on the extracted frames.  
This step automatically detects pupae appearances and creates an **inference results file** that contains all detections.

---

### **3. Load Data and Images**

Next, open the **Data and Images** tab to load:

🪰  The folder containing the extracted image frames, and  
🪰  The corresponding inference results file.  

Detected pupae are overlaid on the images, allowing you to visually inspect and assess the detection quality.

---

### **4. Review and Refine Detections**

This is the most detailed stage and uses both the **Browse Inferences** and **Individual Pupa Browser** panels.

#### **4.1. Automated ID Assignment and Filtering**

Use **Load/Analyse Input File** to:

🪰  Assign IDs to each detected pupa,  
🪰  Propagate IDs across frames, and  
🪰  Apply filters to clean up detections.

Key parameters:

🪰  **Merging IoU Threshold:** Defines the overlap (intersection-over-union) required to merge bounding boxes across frames.  
🪰  **Minimum Appearances per ID:** Filters out IDs that appear fewer than the specified number of times, helping to remove spurious detections.

#### **4.2. Manual Validation**

Manually verify and adjust detections to ensure accurate identification and first appearance timing.  

Use:

🪰  The **Browse Panel** and **Single Pupa Panel** to inspect individual pupae by ID,  
🪰  The **Move First Appearance** button and slider to fine-tune timing, and  
🪰  The deletion tools to remove false detections.

---

### **5. Export Results**

Once you’re satisfied with the validated detections:

1. Add your **metadata** in the **Export** tab.  
2. Use the **split slider** to define the left and right sides of the terrarium.  
3. Click **Export** to generate the final Excel output file.

The exported file will be saved in your working directory under the folder:

