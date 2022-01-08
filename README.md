# PaintGUI
Paint application based on the OCaml GUI toolkit, with added widget features controlling color changes and line thickness. Completed as a homework project in CIS120 at UPenn.

## Class Overview
#### Paint
The paint program uses a mutable record to store its state, which consists of the sequence of shapes drawn (state.shapes), the input mode of the paint program (state.mode), and the selected pen color (state.color). The GUI interface consists of the canvas, mode_toolbar, and color_toolbar.

#### Widget
A widget is a record with three fields that are all functions: they tell the widget how to draw itself (repaint), how to handle events (handle), and how big it is (size).

#### Event Handling
All mouseclicks on the canvas are handled via the paint_action function. The canvas controller (paint_canvas_controller) adds this function to the canvas' event_listeners resulting in the paint_action function being called for every mouse click.

To handle mouseclicks, there are three mouse modes: mouse down, mouse drag, and mouse up. The handling of these events depends on the selected mode on the toolbar, and is written in paint_action. Each shape drawn is added to a deque which is accessed to display the updated canvas.
