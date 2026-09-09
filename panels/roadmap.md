# PANELS

Parametric panel layout generator for AutoCAD.

License: CC BY-NC-SA 4.0  
https://creativecommons.org/licenses/by-nc-sa/4.0/

---

# Goal

Generate panel layouts inside a rectangular boundary using modular rules.

The rectangle is fixed first.

The user then adjusts panel sizes and gaps while previewing the result.

Final geometry is created using scalable block references rather than individual polylines.

---

# Command

```text
PANELS
```

---

# Workflow

## Step 1

Select overall boundary.

```text
Pick first corner:
Pick opposite corner:
```

The overall width and height are now fixed.

---

## Step 2

Specify module size.

Preferred workflow:

```text
Panel Size [Pick] <900,700>:
```

Allowed inputs:

```text
900,700
```

```text
@900,700
```

or graphical pick:

```text
Pick first corner:
Pick opposite corner:
```

Result:

```text
Panel Width  = 900
Panel Height = 700
```

---

## Step 3

Specify gaps.

Preferred workflow:

```text
Gap X,Y <20,20>:
```

Examples:

```text
20,20
```

```text
30,15
```

---

## Step 4

Choose layout mode independently on X and Y.

### X Mode

```text
Equal
FixedPanel
FixedGap
FixedPanelGap
```

### Y Mode

```text
Equal
FixedPanel
FixedGap
FixedPanelGap
```

Example:

```text
X Mode = FixedPanelGap
Y Mode = Equal
```

---

## Step 5

Preview loop.

Displayed values:

```text
Boundary Width

Boundary Height

Panel Width
Panel Height

Gap X
Gap Y

Columns
Rows

Edge X
Edge Y
```

Commands:

```text
PX
PY

GX
GY

MX
MY

Accept
Cancel
```

---

# Layout Modes

---

## Equal

Inputs:

```text
Count
Gap
```

Output:

```text
All panels equal
```

Example:

```text
D-A-D-A-D-A-D-A-D
```

---

## FixedPanel

Inputs:

```text
Panel Size
```

Output:

```text
Gap automatically calculated
```

Example:

```text
A-D-A-D-A-D-A
```

---

## FixedGap

Inputs:

```text
Gap
```

Output:

```text
Panel size automatically calculated
```

Example:

```text
D-A-D-A-D-A-D
```

---

## FixedPanelGap

Inputs:

```text
Panel Size = B
Gap = D
```

Output:

```text
D-A-D-B-D-B-D-B-D-A-D
```

Where:

```text
A = symmetric edge panels
B = fixed center panels
D = fixed gap
```

Example:

```text
20-640-20-900-20-900-20-900-20-640-20
```

---

# Edge Policy

Current version:

```text
SYMMETRIC
```

Example:

```text
A-B-B-B-A
```

Future:

```text
HALF
FULL
CUSTOM
```

---

# Geometry Engine

Each axis must be solved independently.

Example:

```lisp
(setq xPanels
      '(640 900 900 900 640))
```

```lisp
(setq yPanels
      '(450 1200 1200 450))
```

The grid generator combines both lists.

---

# Block Strategy

Final geometry must use blocks.

No polylines should be created in production mode.

---

## Block Name

```text
PNL_PANEL
```

---

## Base Point

```text
0,0,0
```

---

## Geometry

```text
Width  = 1
Height = 1
```

Centered on origin.

Coordinates:

```text
-0.5,-0.5
+0.5,-0.5
+0.5,+0.5
-0.5,+0.5
```

---

## Properties

All geometry must be:

```text
Layer      = 0
Color      = BYBLOCK
Linetype   = BYBLOCK
Lineweight = BYBLOCK
Transparency = BYBLOCK
```

---

# Block Validation

When PANELS starts:

## Case 1

```text
PNL_PANEL not found
```

Action:

```text
Create block
```

---

## Case 2

```text
PNL_PANEL found
```

Action:

```text
Verify block signature
```

Future implementation:

```text
XDATA
or
hidden attributes
```

---

## Case 3

```text
PNL_PANEL exists but differs from PANELS standard
```

Action:

```text
Create

PNL_PANEL_001
```

or next available name.

Never overwrite user blocks.

---

# Block Insertion

Insertion point:

```text
Panel center
```

Scale:

```text
ScaleX = panel width
ScaleY = panel height
ScaleZ = 1
```

Example:

```text
Panel = 900 x 700

Insert:
ScaleX = 900
ScaleY = 700
ScaleZ = 1
```

---

# Future Attributes

Invisible attributes:

```text
PNL_TYPE
PNL_VERSION
PNL_CODE
PNL_MATERIAL
PNL_FINISH
```

---

# Version Roadmap

## v0.1.0

- FixedPanelGap
- Polylines

## v0.2.0

- X/Y independent solver
- Block instances

## v0.3.0

- Persistent settings
- Block validation
- Preview

## v0.4.0

- Equal
- FixedPanel
- FixedGap

## v1.0.0

- Full preview system
- Dynamic editing
- Attributes
- Production ready