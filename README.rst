th
==

th is a command-line todo list manager in the style of t_ written in Haskell.

th shares with t_ the same features:

- It's simple.

- It just uses a *human readable* plain file.

- It works nicely with Dropbox, control version systems and shell scripts.


Installing th
-------------

th requires `Haskell Platform`_ and a UNIX-like shell (I use zsh). It may work on Linux, OS X and Windows (using Cygwin_).

Once you have installed `Haskell Platform`_, you have to `download <https://github.com/damianfral/th/archive/master.zip>`_ the latest version or clone the repository using git, ``git clone https://github.com/damianfral/th.git``.

Having the repository downloaded/clone, you just need to execute ``cabal install``. You may set up your path to be able to access ``~/.cabal/bin/`` without the absolute path.


Using th
--------

You can execute ``th -h`` to show the usage information::

	th -h
	Usage: th [-v|--version] [-f|--file ARG] [COMMAND]
	A todo list manager written in Haskell

	Available options:
	-h,--help                Show this help text
	-v,--version             Show version
	-f,--file ARG            Specify filename [default = todo.txt]

	Available commands:
	create                   Create a new task
	start                    Start a task
	finish                   Finish a task
	delete                   Delete a task

Each task can be in one of these 3 states: *not done*, *started*, *finished*.


Create tasks
++++++++++++

::
			
	> th create "Release version v1.6"

	1 - [ ] Release version v1.6


	> th create "Review open issues"

	1 - [ ] Release version v1.6
	2 - [ ] Review open issues


Start/Finish a task
+++++++++++++++++++

::

	> th start 1

	1 - [-] Release version v1.6
	2 - [ ] Review open issues

	> th finish 1

	1 - [x] Release version v1.6
	2 - [ ] Review open issues


Delete a task
+++++++++++++

::

	> th delete 1

	1 - [ ] Review open issues


Show tasks
++++++++++

::

	> th

	1 - [ ] Review open issues

Autocompletion
--------------

::

	> th --bash-completion-script `which th`


.. _`Haskell Platform`: http://www.haskell.org/platform/index.html
.. _Cygwin: http://www.cygwin.com/
.. _t: http://stevelosh.com/projects/t/
