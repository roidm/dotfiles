from libqtile import layout
from settings.theme import colors

# Layouts and layout rules


layout_conf = {
    'border_focus': colors['colorA'][0],
    'border_normal': colors['inactive2'][0],
    'border_width': 3,
    'margin': 15
}

layouts = [
    layout.MonadTall(**layout_conf),
    layout.Max(),
    layout.MonadWide(**layout_conf),
    layout.Bsp(**layout_conf),
    layout.Matrix(columns=2, **layout_conf),
    layout.RatioTile(**layout_conf),
    layout.Stack(num_stacks=3, **layout_conf),
    # layout.Columns(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

floating_layout = layout.Floating(
    float_rules=[
        {'wmclass': 'confirm'},
        {'wmclass': 'dialog'},
        {'wmclass': 'download'},
        {'wmclass': 'error'},
        {'wmclass': 'file_progress'},
        {'wmclass': 'notification'},
        {'wmclass': 'splash'},
        {'wmclass': 'toolbar'},
        {'wmclass': 'confirmreset'},
        {'wmclass': 'makebranch'},
        {'wmclass': 'maketag'},
        {'wname': 'branchdialog'},
        {'wname': 'pinentry'},
        {'wmclass': 'ssh-askpass'},
    ],
    border_focus=colors["color4"][0]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
