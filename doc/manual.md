Orcabot Manual
==============

Base Module
-----------

The base module does not need to be explicitly added to the module
list.  It always has the highest priority of any other module.  It
handles the access list and command parsing, as well as the required
functionality for an IRC bot.

### Commands

* `about` - Emits a short blurb about orcabot and lists all the enabled modules
* `help` - Gives a list of commands and the first line of their docstrings

### Configuration

* `(nickname <nick>)` -
   Sets the nickname of the bot.  This should always be set.  If the
   nickname cannot be used on connection, the bot will attempt to grab
   the nickname as soon as it can.
   
* `(autojoin <channels>)` - Channels which the bot joins upon connection to the server
   
   
Admin Module
------------

The admin module contains the sort of commands that you wouldn't want
others to run.  Typically, the use the whole admin module is
restricted via the ACL.

### Commands

* `echo <stuff>` - Makes orcabot say something in the same channel
* `action <stuff>` - Makes orcabot perform an action in the same channel
* `sayto <target> <stuff>` - Makes orcabot say something to a channel or user
* `ignore <targets>` - Adds a deny rule to the acl targeting the user.  Essentially cuts
* `unignore <targets>` - Removes blanket deny rules targeting the user.
* `join <channels>` - Causes the bot to join the channels
* `part <channels>` - Causes the bot to leave the channels
* `nick <new nick>` - Causes the bot to change nicks
* `quit` - Causes the bot to disconnect from the IRC server.
* `eval` - Causes the bot to evaluate a Common Lisp expression.  This can be
   used to alter the code of the bot itself, and should not be shared
   freely.

Automsg Module
--------------

Sends a private message to anyone joining a channel.

### Configuration

    (automsg ("channel" . "message")
             ("channel" . "message"))
    
Bugzilla Module
---------------

Scans the conversation looking for mentions of bug numbers.  Prints
out a short summary of any bugs mentioned along with a link.

### Commands

* `bug <bug number> [...]` - Prints out a summary of the bug

### Configuration

The bugzilla module requires a .netrc file in the home directory in
order to login to the bugzilla site.

    (bugzilla :base-url <base bugzilla url>)

Chant Module
------------

The chant module scans the text of messages looking for something
appropriate to chant.  When prompted, it chants the last appropriate
thing.

### Commands

* `chant` - sets up the chant

Environment Module
------------------

This module is for negotiating access to shared resources between
multiple people.  The resources here are called 'environments,' but
they could be anything.  Environments have a status and may have a
number of leases on them.  Ordinarily, only one lease is allowed per
environment, but environments may be shared.  Leases are required to
have a finite timeout.

Times are specified in seconds, minutes, hours, or days by suffixing a
number with s, m, h, or d.  If no suffix is specified, minutes are the
default unit.

### Commands

* `env` - Lists the available environments and their statuses
* `env add <environment>` - Adds another environment
* `env remove <environment>` - Removes an environment from the list
* `take <environment> <time> <reason>` - Takes an environment for use.
  This command will fail if a lease already exists on the environment.
* `share <environment> <time> <reason>` - Shares an environment with
  another person, even if they have a lease on it.
* `steal <environment> <time> <reason>` - Takes an environment for
  use, removing any leases that might already be on that environment.
* `release <environment>` - Releases your lease on an environment
* `update <environment> <status>` - Updates the status of the environment

Grammar Module
--------------

This module implements a grammar-based text generator for delivering
funny text.

### Commands

* `manage [<person/thing>]` - Buzzword-compliant management advice
* `solve [<problem>]` - Diagnose and advise on any problem, Sci-fi
  style.
* `insult [<person>]` - Insult yon ruffian in the King's style
* `plot [<person>]` - Generate the plotline to a story
* `food [<person>]` - Throw a random food at someone

Groups Module
-------------

The groups module allows users to set up groups of people which can
then be easily messaged within a channel.  Groups are created by
adding members to them, and deleted by removing all their members.

### Commands

* `group <group>` - displays the online and offline members of the group
* `group <group> <message>` - sends a message to the group
* `group --list` - lists the available groups
* `groupadd <group> <members>` - Adds members to a group
* `groupdel <group> <members>` - Removes members from a group

Karma Module
------------

The karma module tracks "karma" for users, ideas, and anything else.
All things start with a karma of zero.  This module scans for strings
ending or beginning with ++ or --, and increments or decrements the
karma of the target.

### Commands

* `karma <term>` - Outputs the karma of the term

Kittens Module
--------------

This module exists only to express admiration of kittens whenever they
are mentioned.

Lastseen Module
---------------

Tracks the users on the same channel and stores their last activity
along with the time.

### Commands

* `seen <nick>` - Shows the last activity from the nick

Logging Module
--------------

Logs the messages within the configured channels to a local file.

### Configuration

The logging module will support any number of LOG forms in this
format:

    (log <channel> <pathname>)

Note that if the bot has not joined the channel, no logging will take
place.

Memo Module
-----------

Allows people to leave messages for each other.  The messages are sent
via PRIVMSG whenever activity is seen on the target nick.

### Commands

* `memo <nick> <message>` - Leaves a memo for the given nick

Quote Module
------------

Stores notable quotes from the users and displays a random one when
asked.

### Commands

* `quote` - Displays a random quote
* `quote <quote>` - Adds a quote to the quote database

RT Module
---------

This module is an interface to the RT request tracker ticketing
system.  It scans the conversation looking for tix numbers and
displays a short summary of the tix along with a link to the ticket.

### Commands

* `tix <tix number> [...]` - Prints out a short summary of the tickets

### Configuration

The RT module requires a .netrc entry for the tix host in order to
login to the system.

    (rt :base-url <tix base url>)

Stats Module
------------

Tracks statistics for each observed user and channel.

### Commands

* `stat <user>` - Prints out statistics for the given user
* `stat <channel>` - Prints out statistics for the channel

Subversion Module
-----------------

Scans channel conversations for mentions of subversion revisions and
emits a short description plus a link to the revision.  Also provides
an svn command to look up subversion revisions or repository paths
explicitly.

### Commands

* `svn <revision number> [...]` - Prints out a summary of the
  subversion revisions from the repository
* `svn <path>` - Prints out a summary of the last subversion revision
  to modify the path

### Configuration

Specifies the repo url and the url of the link to a related trac
site.  If a .netrc file exists in the home directory, it will be used
for authentication.

    (subversion :repo-url <repo> :trac-url <trac url>)

Trivia Module
-------------

Implements a trivia game.  Players ask for a trivia question with the
trivia command.  The first person to guess it correctly gets a point
(if they haven't answered it already) or keeps the others from getting
a point.

### Commands

* `trivia` - Requests a new trivia question
* `trivia --score` - Displays the user's current score
* `trivia --top` - Displays users with the top 10 scores
* `addtrivia <question>? <answer> [. <answer> ...]` - Add a new trivia question
* `edittrivia <idnum> <question>? <answer> [. <answer> ...]` - Edits
  an existing trivia question
* `deltrivia <idnum>` - Removes a trivia question

Typist Module
-------------

Implements a trivially breakable typing test.  The typist command
causes it to emit a sample of text.  The user attempts to type the
text and the bot calculates the typing speed from the delay.

### Commands

* `typist` - Requests a new typing test

URL Module
------------

Stores notable urls from the users and displays a random one when
asked.

### Commands

* `url` - Displays a random url
* `url <url>` - Adds a url to the url database


Werewolf Module
---------------

Implements a game of Werewolf in IRC.

When a game is started, a certain amount of time is waited for
other players to join.  Once everyone has joined, the game starts.
Everyone is randomly assigned a werewolf or villager side.  The
game starts at night.

At night, the werewolves decide by vote who to kill.  That person
is eliminated from the game.  Night ends once all the votes are
cast.

At day, everyone decides by vote who to lynch.  That person is
eliminated from the game.

The game continues until the werewolves equal the villagers
(werewolves win) or the villagers eliminate all the werewolves
(villagers win)

### Commands

* `ww join` - Joins the werewolf game (only when unstarted)
* `ww leave` - Leaves the werewolf game (any time)
* `ww start` - Starts a new werewolf game with the current players
* `ww list` - Lists players.  If a werewolf, also lists side
* `ww vote <nick>` - Votes on a player to eliminate.  Only werewolves
  can vote at night.
