from libqtile import widget
from settings.theme import colors

# Get the icons at https://www.nerdfonts.com/cheat-sheet (you need a Nerd Font)
myTerm = "alacritty"
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
        padding_y=4,
        padding_x=4,
        borderwidth=1,
        active=colors['active'],
        inactive=colors['active'],
        rounded=False,
        highlight_method='block',
        urgent_alert_method='block',
        urgent_border=colors['urgent'],
        this_current_screen_border=colors['colorA'],
        this_screen_border=colors['grey'],
        other_current_screen_border=colors['dark'],
        other_screen_border=colors['dark'],
        disable_drag=True
    ),
    separator(),
    widget.WindowName(**base(fg='color3'), fontsize=18, padding=6),
    separator(),
]

primary_widgets = [

    widget.Sep(linewidth=0, padding=7, background = colors['dark']),

    widget.Image(scale=0.45, padding=9, filename = "~/.config/qtile/icons/gentoo2.png", mouse_callbacks = {'Button1': lambda qtile:qtile.cmd_spawn('dmenu_run -p "Run: "')}),

    widget.Sep(linewidth=0, padding=3, background = colors['dark']),

    *workspaces(),

    separator(),


    powerline('color3'),

    widget.Net(**base(bg='color3'), interface='enp34s0', format = '↓ {down} ↑ {up}', padding=10, update_interval=1.5),

    powerline('color7', 'color3'),

    icon(bg="color7", fontsize=22, text='﬙ '),

    widget.CPU(foreground = colors['dark'], background = colors['color7'], padding = 10, mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn('gnome-system-monitor')}, update_interval=1.5, format = '{load_percent}%'),

    powerline('color5', 'color7'),

    widget.TextBox(text = " Vol:", foreground = colors['dark'], background = colors['color5'], padding=0),

    widget.Volume(foreground = colors['dark'], background = colors['color5'], padding = 10),

    powerline('color4', 'color5'),

    icon(bg="color4", fontsize=22, text=' '),

    widget.Memory(background=colors['color4'], foreground=colors['dark'], update_interval=2.0, padding=10, mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn(myTerm + ' -e htop')}),

    powerline('color2', 'color4'),

    widget.CurrentLayoutIcon(foreground = colors['dark'], background = colors['color2'], scale=0.55),

    widget.CurrentLayout(foreground = colors['dark'], background = colors['color2'], padding=5),

    powerline('color9', 'color2'),

    icon(bg="color9", fontsize=20, text=' '),

    widget.Clock(foreground = colors['dark'], background = colors['color9'], format='%d/%m/%Y - %H:%M '),

    powerline('dark', 'color9'),

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
    'fontsize': 17,
    'padding': 2,
}
extension_defaults = widget_defaults.copy()
