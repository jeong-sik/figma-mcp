# Supported `figma_plugin` Actions

`figma_plugin` supports actions defined in `lib/mcp_plugin_actions.ml`.

This list is maintained from a single source in code. Unknown actions fail fast with:

`Unknown plugin action '<action>'.`

No fuzzy/heuristic action suggestions are appended.

## Core actions

- connect
- use_channel
- status
- read_selection
- get_node
- export_image
- get_variables
- apply_ops
- list_pages
- switch_page
- list_components
- clone
- group
- ungroup
- set_selection
- zoom_to
- reorder
- set_locked
- set_visible
- flatten
- set_auto_layout
- get_viewport
- set_viewport
- rename
- resize
- move
- set_opacity
- set_corner_radius
- set_fill
- set_stroke
- set_effects
- create_component
- create_instance
- detach_instance
- set_text
- find_all
- notify
- create_frame
- create_rectangle
- create_ellipse
- create_text
- create_line
- create_polygon
- create_star
- delete_node
- duplicate
- align
- distribute
- boolean_union
- boolean_subtract
- boolean_intersect
- boolean_exclude
- get_local_styles
- set_constraints
- create_page
- delete_page
- rotate
- flip
- outline_stroke
- set_blend_mode
- get_selection_colors
- swap_fill_stroke
- copy_style
- get_fonts
- set_parent
- create_vector
- set_image_fill
- get_plugin_data
- set_plugin_data
- get_doc_info
- get_absolute_bounds
- create_component_set
- remove_auto_layout
- create_slice
- set_export_settings
- get_reactions
- set_reactions
- rasterize
- get_shared_plugin_data
- set_shared_plugin_data
- swap_component
- resize_to_fit
- get_characters
- set_range_fills
- set_range_font_size
- insert_child
- get_all_local_variables
- get_styles_by_type
- apply_style
- get_overrides
- reset_overrides
- bring_to_front
- send_to_back
- set_grid
- get_layer_list
- scroll_and_zoom
- get_paint_styles
- set_text_case
- get_stroke_details
- set_stroke_weight
- collapse_layer
- export_viewport
- export_selection
- get_changes
- watch_start
- watch_stop

## Special actions

- batch
- annotate
- subscribe_events
- export_tokens

