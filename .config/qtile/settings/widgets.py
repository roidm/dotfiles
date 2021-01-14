from libqtile import widget
from settings.theme import colors

# Get the icons at https://www.nerdfonts.com/cheat-sheet (you need a Nerd Font)

base = lambda fg='text', bg='dark': {
    'foreground': colors[fg],
    'background': colors[bg]
}

separator = lambda: widget.Sep(**base(), linewidth=0, padding=5)

icon = lambda fg='text', bg='dark', fontsize=23, text="?": widget.TextBox(
    **base(fg, bg),
    fontsize=fontsize,
    text=text,
    padding=3
)

powerline = lambda fg="light", bg="dark": widget.TextBox(
   **base(fg, bg),
    text="", # Icon: nf-oct-triangle_left
    fontsize=50,
    padding=-4
)

workspaces = lambda: [
    separator(),
    widget.GroupBox(
        **base(fg='light'),
        font='UbuntuMono Nerd Font',
        fontsize=23,
        margin_y=2,
        margin_x=2,
        padding_y=2,
        padding_x=2,
        borderwidth=1,
        active=colors['active'],
        inactive=colors['active'],
        rounded=False,
        highlight_method='block',
        urgent_alert_method='block',
        urgent_border=colors['urgent'],
        this_current_screen_border=colors['focus'],
        this_screen_border=colors['grey'],
        other_current_screen_border=colors['dark'],
        other_screen_border=colors['dark'],
        disable_drag=True
    ),
    separator(),
    widget.WindowName(**base(fg='active2'), fontsize=18, padding=6),
    separator(),
]

primary_widgets = [
    
    widget.Image(
        scale=0.35,
        padding=5,
        filename = "~/.config/qtile/icons/bar.png"
    ), 

    widget.Image(
        scale=0.38,
        padding=5,
        filename = "~/.config/qtile/icons/gentoo.png",
        mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn('dmenu_run')}
    ), 
    
    widget.Image(
        scale=0.35,
        padding=5,
        filename = "~/.config/qtile/icons/bar.png"
    ), 
    
    *workspaces(),

    separator(),

   # powerline('color4', 'dark'),

    #icon(bg="color4", text=' '), # Icon: nf-fa-download
    
   # widget.Emerge(**base(bg='color4'), update_interval=1800),

    powerline('color3'),

    icon(bg="color3", text=' '),  # Icon: nf-fa-feed
    
    widget.Net(**base(bg='color3'), interface='enp34s0'),
    
    

    powerline('color2', 'color3'),

    widget.CurrentLayoutIcon(**base(bg='color2'), scale=0.55),

    widget.CurrentLayout(**base(bg='color2'), padding=5),
    
    powerline('color1', 'color2'),

    icon(bg="color1", fontsize=22, text=' '), # Icon: nf-mdi-calendar_clock

    widget.Clock(**base(bg='color1'), format='%d/%m/%Y - %H:%M '),

    powerline('dark', 'color1'),

    widget.Systray(background=colors['dark'], padding=5),

]

secondary_widgets = [
    *workspaces(),

    separator(),

    powerline('color1', 'dark'),

    widget.CurrentLayoutIcon(**base(bg='color1'), scale=0.65),

    widget.CurrentLayout(**base(bg='color1'), padding=5),
]

widget_defaults = {
    'font': 'UbuntuMono Nerd Font Bold',
    'fontsize': 16,
    'padding': 1,
}
extension_defaults = widget_defaults.copy()
