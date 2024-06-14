# poltergeist

A bare-bones remote keyboard and mouse server.

Poltergeist only works on X11, not Wayland. If/when I revisit this project, I
will probably use Linux's uinput instead, which is independent of any graphical
server.

Please note that poltergeist does not encrypt the connection or perform any kind
of authentication. SSH port-forwarding would be a good solution for both
problems. If you'd like something more robust and feature-complete, I'd
recommend looking into X server forwarding or KDE connect
(https://kdeconnect.kde.org).

## Usage

Poltergeist will accept commands to use the mouse and keyboard from other
devices on the network, allowing them to control it.

Note that no clients/libraries are included, but the protocol used is
dead-simple. To send commands, you just send them as plaintext in a UDP packet
to poltergeist. You can do so in shell scripts with netcat, i.e.:

```
echo -n "<COMMAND>" | nc -u <DEVICE IP ADDRESS> <POLTERGEIST PORT>
```

`-n` means to not add a newline, `-u` means netcat runs connects over UDP
instead of TCP.

Creating a UDP connection in most programming languages should be pretty easy if
you don't want to use netcat.

There are three commands, `movemouse`, `click`, and `presskey`, which move the
mouse, perform mouse clicks, and perform keypresses respectively.

#### movemouse

`movemouse` expects two integers as arguments, the first being the pixels to
move on the x-axis and the second being the y-axis. Positive values mean up and
to the right, negative values move opposite. I.e., to move the mouse 400 pixels
up and 400 to the right:

```
movemouse 400 400
```

#### click

`click` expects a mouse button to click and an optional list of modifier keys to
press with it. The button can either be `left`, `middle`, or `right`. The
modifier keys can be any of `control`, `alt`, `shift`, `super`, `meta`. I.e., to
preform a control + left click:

```
click left control
```

#### presskey

`presskey` expects a key to press and an optional list of modifier keys to press
with it. The list of keys is based on the US keyboard, see the `is_key/1`
function in `src/poltergeist_control_server.erl` for all available keys. The
modifier keys can be any of `control`, `alt`, `shift`, `super`, `meta`. I.e., to
press control + alt + t, which on many desktop environments will open a
terminal:

```
presskey t control alt
```

## How to build

You will need Erlang/OTP (https://www.erlang.org) and xdotool
(https://github.com/jordansissel/xdotool) installed on your system. There is a
`flake.nix` you can use with `nix develop path:.` to get them.

Then run the following command to build everything:

```
./emake.escript
```

Note that this will also build the PLT file used to do static analysis tests,
which may take a minute. If you're impatient, run this command instead:

```
./emake.escript build
```

## How to run

To run poltergeist, enter the Erlang shell in the project directory with the
following command:

```
erl -pa ebin/ -enable-feature maybe_expr
```

`-enable-feature maybe_expr` is required for Erlang/OTP version < 27.

Then run the following command in the shell:

```
application:start(poltergeist).
```

Once poltergeist is finished starting up it will be made available on port
`12020`.

The following command can be used to turn it off gracefully:

```
application:stop(poltergeist).
```

If you would like to change the port, modify `poltergeist.config` and add
`-config poltergeist.config` to the command to launch the Erlang shell, i.e.:

```
erl -pa ebin/ -enable-feature maybe_expr -config poltergeist.config
```
