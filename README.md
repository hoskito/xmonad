# xmonad
A dynamically tiling X11 window manager that is written and configured in Haskell.


## Table of contents
1. [Key bindings](#key-bindings)
2. [Links](#Links)
3. [Screenshots](#Screenshots)


## Links
- [xmonad tiling window manager](https://xmonad.org/)
- [xmobar](https://hackage.haskell.org/package/xmobar)


## Key bindings

### Action key bindings

| Key binding                                                  | Action                                                                        |
|--------------------------------------------------------------|-------------------------------------------------------------------------------|
| <kbd>mod</kbd> - <kbd>return</kbd>                           | Launch terminal                                                               |
| <kbd>mod</kbd> - <kbd>p</kbd>                                | Show dmenu                                                                    |
| <kbd>mod</kbd> - <kbd>backspace</kbd>                        | Close focused window                                                          |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>backspace</kbd>     | Close all windows on workspace (prompt)                                       |
| <kbd>mod</kbd> - <kbd>q</kbd>                                | Restart xmonad (prompt)                                                       |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>q</kbd>             | Quit xmonad (prompt)                                                          |

### Movement key bindings

| Key binding                                                  | Action                                                                        |
|--------------------------------------------------------------|-------------------------------------------------------------------------------|
| <kbd>mod</kbd> - <kbd>space</kbd>                            | Rotate through layout                                                         |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>space</kbd>         | Reset the layouts on the current workspace to default                         |
| <kbd>mod</kbd> - <kbd>tab</kbd>                              | Move focus to the next window                                                 |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>tab</kbd>           | Move focus to the previous window                                             |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>tab</kbd>           | Move focus to the previous window                                             |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>tab</kbd>           | Move focus to the previous window                                             |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>tab</kbd>           | Move focus to the previous window                                             |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>j</kbd>             | Swap focused window with next window                                          |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>\<Down\></kbd>      | Swap focused window with next window                                          |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>k</kbd>             | Swap focused window with prev window                                          |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>\<Up\></kbd>        | Swap focused window with prev window                                          |
| <kbd>mod</kbd> - <kbd>a</kbd>                                | Move focused window to master                                                 |
| <kbd>mod</kbd> - <kbd>m</kbd>                                | Move focus to master window                                                   |
| <kbd>mod</kbd> - <kbd>,</kbd>                                | Rotate all windows except master                                              |
| <kbd>mod</kbd> - <kbd>.</kbd>                                | Rotate all windows current stack                                              |
| <kbd>mod</kbd> - <kbd>f</kbd>                                | Fullscreen                                                                    |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>\<Page_Up\></kbd>   | Move window to next WS                                                        |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>\<Page_Down\></kbd> | Move window to prev WS                                                        |

### Window resizing

| Key binding                                                  | Action                                                                        |
|--------------------------------------------------------------|-------------------------------------------------------------------------------|
| <kbd>mod</kbd> - <kbd>[</kbd>                                | Shrink window                                                                 |
| <kbd>mod</kbd> - <kbd>]</kbd>                                | Expand window                                                                 |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>[</kbd>             | Shrink window vertically                                                      |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>]</kbd>             | Expand window vertically                                                      |

### Floating windows

| Key binding                                                  | Action                                                                        |
|--------------------------------------------------------------|-------------------------------------------------------------------------------|
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>f</kbd>             | Toggle float layout                                                           |
| <kbd>mod</kbd> - <kbd>t</kbd>                                | Sink a floating window                                                        |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>t</kbd>             | Sink all floated windows                                                      |

### Window spacing (gaps)

| Key binding                                                  | Action                                                                        |
|--------------------------------------------------------------|-------------------------------------------------------------------------------|
| <kbd>ctrl</kbd> - <kbd>mod-alt</kbd> - <kbd>j</kbd>           | Decrease window spacing                                                       |
| <kbd>ctrl</kbd> - <kbd>mod-alt</kbd> - <kbd>k</kbd>           | Increase window spacing                                                       |
| <kbd>ctrl</kbd> - <kbd>mod-alt</kbd> - <kbd>h</kbd>           | Decrease screen spacing                                                       |
| <kbd>ctrl</kbd> - <kbd>mod-alt</kbd> - <kbd>l</kbd>           | Increase screen spacing                                                       |


## Screenshots

![Screenshot 1](/../screenshots/images/screen-01.jpg?raw=true "Screenshot 1")

![Screenshot 2](/../screenshots/images/screen-02.jpg?raw=true "Screenshot 2")

![Screenshot 3](/../screenshots/images/screen-03.jpg?raw=true "Screenshot 3")

![Screenshot 4](/../screenshots/images/screen-04.jpg?raw=true "Screenshot 4")