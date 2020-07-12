# Install Instructions

The pre-compiled binary for Linux works on a very wide range of distributions.

It should be possible to install it by running the following commands in your terminal:

```bash
# Move to your Desktop so you can see what is going on easier.
#
cd ~/Desktop/

# Download the 0.1.0 binary for Linux.
#
# +----------------+---------------------------+
# | FLAG           | MEANING                   |
# +----------------+---------------------------+
# | -L             | follow redirects          |
# | -o elm2node.gz | name the file elm2node.gz |
# +----------------+---------------------------+
#
curl -L -o elm2node.gz https://github.com/sebsheep/elm2node/releases/download/0.1.0-alpha-2/elm2node.gz

# There should now be a file named `elm2node.gz` on your Desktop.
#
# The downloaded file is compressed to make it faster to download.
# This next command decompresses it, replacing `elm.gz` with `elm`.
#
gunzip elm2node.gz

# There should now be a file named `elm2node` on your Desktop!
#
# Every file has "permissions" about whether it can be read, written, or executed.
# So before we use this file, we need to mark this file as executable:
#
chmod +x elm2node

# The `elm2node` file is now executable. That means running `~/Desktop/elm2node --help`
# should work. Saying `./elm2node --help` works the same.
#
# But we want to be able to say `elm2node --help` without specifying the full file
# path every time. We can do this by moving the `elm2node` binary to one of the
# directories listed in your `PATH` environment variable:
#
sudo mv elm2node /usr/local/bin/

# Now it should be possible to run the `elm2node` binary just by saying its name!
#
elm2node --help
```

<br/>

## Wait, what is the `PATH` variable?

When you run a command like `elm2node src/Main.elm`, your computer starts by trying to find an executable file called `elm2node`.

The `PATH` is the list of directories that get searched. You can see these directories by running:

```bash
echo $PATH
```

This prints `/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin` on my computer. The directories are separated by a `:` so there are five possibilities listed here.

When I run `elm2node src/Main.elm`, my terminal starts by searching these five directories for an executable file named `elm2node`. It finds `/usr/local/bin/elm2node` and then runs `/usr/local/bin/elm2node src/Main.elm` with whatever arguments I gave.

So the `PATH` environment variable is a convention that allows you to refer to a specific executable file without knowing exactly where it lives on your computer. This is actually how all "terminal commands" work! Commands like `ls` are really executable files that live in directories listed in your `PATH` variable.

So the point of running `sudo mv elm2node /usr/local/bin/` is to turn the `elm` binary into a terminal command, allowing us to call it just like `ls` and `cd`.

**Note:** Why do we need to use `sudo` for that one command? Imagine if some program was able to add executables named `ls` or `cd` to `/usr/local/bin` that did something tricky and unexpected. That would be a security problem! Many distributions make this scenario less likely by requiring special permissions to modify the `/usr/local/bin/` directory.


<br/>

## Uninstall

The following command should remove everything:

```bash
# Remove the `elm2node` executable.
#
sudo rm /usr/local/bin/elm2node
```

<br/>

## I did'nt write these instructions!

These instructions are almost the same than [the ones to install the official elm compiler](https://github.com/elm/compiler/blob/master/installers/linux/README.md),
I've just replace `elm` by `elm2node` at some places.

I think these instructions are very well written and would looooove that all install instructions
being so nice and instructive, instead of cryptic bash commands!

So thank you Evan for those wonderfull instructions!