# xmonad
A dynamically tiling X11 window manager that is written and configured in Haskell.

![Screenshot 1](/../screenshots/images/screen-01.jpg?raw=true "Screenshot 1")

![Screenshot 2](/../screenshots/images/screen-02.jpg?raw=true "Screenshot 2")

![Screenshot 3](/../screenshots/images/screen-03.jpg?raw=true "Screenshot 3")

![Screenshot 4](/../screenshots/images/screen-04.jpg?raw=true "Screenshot 4")


## Table of contents
1. [Read more about](#Read-more-about)
2. [Key bindings](#key-bindings)


### Read more about:
- [xmonad tiling window manager](https://xmonad.org/)
- [xmobar](https://hackage.haskell.org/package/xmobar)


### Key bindings:

## Action key bindings

| Key binding                                             | Action                                                                        |
|---------------------------------------------------------|-------------------------------------------------------------------------------|
| <kbd>mod</kbd> - <kbd>return</kbd>                      | Launch terminal                                                               |
| <kbd>mod</kbd> - <kbd>p</kbd>                           | Show dmenu                                                                    |
| <kbd>mod</kbd> - <kbd>backspace</kbd>                   | Close focused window                                                          |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>backspace</kbd>| Close all windows on workspace(prompt)                                        |
| <kbd>mod</kbd> - <kbd>q</kbd>                           | Restart xmonad                                                                |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>q</kbd>        | Quit xmonad                                                                   |

## Movement key bindings

### Window Movement key bindings

| Key binding                                             | Action                                                                        |
|---------------------------------------------------------|-------------------------------------------------------------------------------|
| <kbd>mod</kbd> - <kbd>space</kbd>                       | Rotate through layout                                                         |
| <kbd>mod</kbd> - <kbd>shift</kbd>   - <kbd>space</kbd>  | Reset the layouts on the current workspace to default                         |
| <kbd>mod</kbd> - <kbd>tab</kbd>                         | Move focus to the next window                                                 |
| <kbd>mod</kbd> - <kbd>shift</kbd>   - <kbd>tab</kbd>    | Move focus to the previous window                                             |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>j</kbd>        | Move focus to the next window                                                 |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>\<Down\></kbd> | Move focus to the next window                                                 |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>k</kbd>        | Move focus to the previous window                                             |
| <kbd>mod</kbd> - <kbd>shift</kbd> - <kbd>\<Up\></kbd>   | Move focus to the previous window                                             |
