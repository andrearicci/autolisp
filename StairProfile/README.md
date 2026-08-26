## StairProfile.lsp

Generates a stair section from two picked points.
The stair is drawn as a lightweight polyline and can optionally generate a nosing and an MTEXT design report.

More info:
[https://andrearicci.it/howtocad/tools/stairprofile/](https://andrearicci.it/howtocad/tools/stairprofile/)

[Demo on YT](https://youtu.be/ZwdLpNA_qJ4?si=dHSSVCVvAazjSwQN)

### COMMAND

 command: __STAIR__

### WORKFLOW

 1) Pick Base Point
 2) Pick Arrival Point
 3) Review Preview
 4) Modify stair parameters if needed
 5) Accept or Exit


### MAIN MENU

 ``[+/-/Tread/Nosing/Accept/Exit] <Accept>``


| Action | Description |
| :--- | :--- |
| **+** | Add one riser |
| **-** | Remove one riser |
| **Tread** | Tread settings |
| **Nosing** | Nosing settings |
| **Accept** | Create final geometry |
| **Exit** | Cancel command |


#### MENU TREE

    STAIR
    ├─ Pick Base Point
    ├─ Pick Arrival Point
    │
    ├─ Main Menu
    │   ├─ +
    │   │   └─ Add riser
    │   │
    │   ├─ -
    │   │   └─ Remove riser
    │   │
    │   ├─ Tread
    │   │   ├─ Value
    │   │   │   └─ Fixed tread value
    │   │   │
    │   │   ├─ Ergonomic
    │   │   │   └─ Automatic Blondel rule
    │   │   │
    │   │   ├─ Fit
    │   │   │   └─ Fit stair to picked run
    │   │   │
    │   │   └─ Accept
    │   │
    │   ├─ Nosing
    │   │   ├─ None
    │   │   │
    │   │   ├─ Square
    │   │   │   ├─ Nosing X
    │   │   │   └─ Nosing Y
    │   │   │
    │   │   ├─ Round
    │   │   │   └─ Diameter
    │   │   │
    │   │   └─ Cancel
    │   │
    │   ├─ Accept
    │   │   ├─ Final geometry
    │   │   └─ Optional MTEXT report
    │   │
    │   └─ Exit
    │       └─ Delete preview
    │
    └─ End


### TREAD MODES

#### ERGONOMIC

Tread automatically calculated using:  
``2R + T ≈ 63 cm``  
scaled to drawing INSUNITS (works in inches, feets, millimeters and meters).

#### FIXEDTREAD

User specifies a tread value.  
Same value is remembered between sessions.

#### FIT

Total run is constrained by picked points.  
Formula:  
``tread = totalRun / (risers - 1)``  
where:  
``totalRun = abs(ep.x - bp.x)``  


### NOSING TYPES

#### NONE
Standard stair profile.

#### SQUARE
Rectangular nosing.

Parameters:  
- X projection
- Y drop


#### ROUND
Semicircular nosing.

Parameter:  
- Diameter

### REPORT
Preview report is shown in the prompt during editing with ⚠ alert if 50° < angle <20°.  
Sample:  
``Preview -> 14 risers of 11.96 | 13 treads of 39.08 | Height 167.46 | Run 508.01 | 2R+T 63.00 | Tread ERGONOMIC | Nosing SQUARE | Angle 18.2° ⚠``

Final report contains:

- Height
- Run
- Number of risers
- Number of treads
- Rise value
- Tread value
- 2R+T
- Stair angle


### MTEXT REPORT
(Optional)

Automatically inserted:

- Above last step
- UCS aligned
- Height = rise / 3
- Remembers Yes/No preference
- ⚠ alert if 50° < angle <20°


### NOTES

- UCS independent.
- _insunits_ independent.
- Run direction automatically detected.
- Preview geometry and parameters.
- Accept promotes preview to final geometry.
- Exit deletes preview geometry.