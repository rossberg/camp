   ___    ___  __  __  ____
  / _ \  /   ||  \/  ||  _ \   |\
 / / \_\/ /| ||      || | \ \  | \
/ /    / /_| || |\/| || |_/ /  |  \
\ \   / ___  || |  | ||  __/   |  /
 \ \_/ /   | || |  | || |      | /
  \___/    |_||_|  |_||_|      |/
   _                    _ 
  / \_/ A MUSIC PLAYER / \_/


Screenshot Gallery: https://github.com/rossberg/camp/blob/master/img/README.md

 _____________________________________________________________________________
|  1. INTRODUCTION                                                            |
|_____________________________________________________________________________|

What you got here is an old-school music player heavily inspired by good old
Winamp [1], with a focus on decent music library and playlist handling.

If you have not yet been sucked in by the streaming cartells, then you might
find it useful.

Camp is entirely written in OCaml [2] and portable across Windows, Mac and
Linux.

I use it every day, but there may be bugs. You have been warned.

This README is intentionally written in retro style.


[1] https://en.wikipedia.org/wiki/Winamp
[2] https://ocaml.org


 _____________________________________________________________________________
|  2. INSTALLATION                                                            |
|_____________________________________________________________________________|

2.1 Requirements
----------------

You'll need OCaml 5.04 or greater and Opam along with it.

On Windows, no way around Cygwin and make. And you'll probably need to manually
install the mingw64-x86_64-winpthreads package to have libwinpthread.dll in the
path, although to be honest, I never quite understand why that is so.

Camp is tested on Windows and Mac. It should work on Linux, too, but I haven't
had a chance to try.


2.2 Building
------------

In the main directory (where you found this file), invoke:

  +---------------
  | make
  +---------------

The first time round, it may trigger the installation of additional Opam
packages, which you'll have to confirm.

On Windows and Linux, the outcome is a stand-alone directory `Camp` bundling
the executable with all files necessary. On Mac, you'll get a proper application
`Camp.app`. In both cases, the result is self-contained and can be moved to
any hard drive location of your chosing.

If you prefer to have either packaged up as a zip file, then go with:

  +---------------
  | make zip
  +---------------

The zip archive will include a suitable version name, too.

Finally, if you're playing around and just want to build the executable in the
current directory, do:

  +---------------
  | make exe
  +---------------


2.3 Running
-----------

Just click the thing! (*)

Or, if you love your keyboard, start it from the command line. You can pass
audio file names to play or queue up that way.

Of course, you can also create file associations for the audio formats you want
to play with this decent piece of software. Then opening such a file will queue
it up in the playlist.

Camp (usually) detects if it is already running. Any files passed to it will
then be added to the current playlist.


(*) If you are on MacOS 26.2+ and witness Camp being unable to open some music
    files, e.g., from the desktop, even though you allowed it before, then this
    is because apparently Apple thinks all users are idiots and prevents it. In
    that case move Camp.app to the Applications folder and start it from there.


2.4 Application Storage
-----------------------

When starting Camp for the first time (no matter from where), it will create a
cosy little directory for itself, where it stores its configuration and
database. Depending on your operating system, this directory is located in the
canonical location for local user application data:

* on Windows, find it under C:\Users\<you>\AppData\Local\Camp,
* on Mac, it'll go to /Users/<you>/Library/Application Support/Camp,
* on Linux, it should be /home/<you>/.local/share/Camp
  (or whatever $XDG_DATA_HOME is set to).

Some of the data there is in text format. You can edit it, but you'll definitely
be on your own.


 _____________________________________________________________________________
|  3. MAKE YOURSELF AT HOME                                                   |
|_____________________________________________________________________________|

3.1 A First Look
----------------

Camp consists of a window with 3 main areas (I call them *panes* below):

* Control: this is where you control playback, volume, etc.,
* Playlist: this is where you queue up tracks,
* Library: this is where you manage your music library.

Playlist and Library are folded away on first start, but you can reveal them by
activating the respective toggles on the right side of the Control pane. That
said, they will look pretty empty at first.

* The Playlist opens below the control pane.

* The Library opens to the right or the left, depending on screen space.

* When the Library is open already, you can Shift-click on the Library toggle
  button to manually switch sides.

* The window can be resized vertically only when the Playlist pane is open,
  and horizontally when the Library pane is open. The library pane will be tiny
  and not very useful when the Playlist is closed.

* Generally, a right-click into most areas of the window will open a nifty
  context menu.

* You quit by pressing the Power button. A Shift-click will minimise the
  window to the taskbar/dock/whatever-it's-called.


3.2 Playing a Track: the Control Pane
-------------------------------------

The usual mode of operation is to drag & drop audio files from your file
system browser to Camp's playlist. Then press the Play button and the joy
begins. Alternatively, use file associations to open audio tracks with Camp.

Most controls around the Playlist should be intuitive. A few extra tips:

* Start/Stop: You can conveniently use the space bar to play/pause/resume.

* Seek: You can skip through the song by clicking/dragging the progress bar.
  Or press left/right cursor keys on the keyboard.

* Volume: Controlled either by using the mouse wheel while hovering over the
  Control pane, or by directly clicking/dragging the triangular volume bar. You
  can also press +/- on the keyboard.

* Mute: There is a red Mute sign below the volume bar that is easy to miss.
  Click it.

* Time: Clicking on the time display toggles between played and remaining time.

* Color: Clicking on the line with the audio details will cycle through several
  color schemes, all inspired by actual HiFi displays from the past. Or use the
  context menu.

* Visualisations: Ctrl/Command-Y cycles through different visualisations of the
  track playing. Either Cover Art (default), Wave Form, Oscilloscope, or None.

* Cover: Clicking on the cover art will zoom it into a pop-up. Click again to
  close.

* Oscilloscope: Dragging the mouse horizontally or vertically on it adjusts
  its scaling in the respective direction.

* Shuffle: Toggles random play of the playlist (Section 3.3).

* Repeat: Cycles Repeat mode between Off (no indicator light), Repeat One (left
  indicator light), and Repeat All (right light).

* Loop: Set AB loop delimiters within a track. On first press, sets loop start
  (left light), on second, sets loop end (right light), on third click
  deactivates looping.

The context menus on the info area or the buttons area reveal additional keys.

If the current track is in the Playlist, its entry is shown in white.

If the current track is in the Library, its home folder is shown in white in
the Browser. Likewise, respective View entries are shown in white. That way, you
can quickly locate it in the library.


3.3 Playing More Tracks: the Playlist
-------------------------------------

I'm sure you have seen playlists before, so hopefully we can make this short.

Mostly, this is operated by drag & drop, e.g., to add files or reorder items.

* Jump: Double-click an entry to play it immediately.

* Select: Click on an entry to select it. Ctrl/Command-click toggles the
  selection of an individual track. Shift-click toggles the selection
  of the entire range between the currently and the last clicked entry.

  Moving with cursor keys changes the current selection to the track
  above/below. With shift, extends the selection/deselection range.

* Reorder: Drag the selection to move it up or down the list. This works even
  when the selection has holes.

  Moving the cursor keys with Ctrl/Command pressed also moves the selection
  up or down.

The edit buttons at the bottom normally act on the current selection, or the
whole playlist if nothing is selected.

* TAG: Invokes an external tag editor. This has to be configured manually,
  my apologies.

  Go to the Application Storage directory (Section 2.4). Open the file
  `state.conf`. Find the entry `config`, and in it the field `exec_tag`.
  Set its string value to the path of the tag editor you want to use, for
  example, I highly recommend Mp3tag [1] on Windows or Mac:

  +------------------------------------------------------
  | exec_tag = "C:\\Program Files\\Mp3tag\\Mp3tag.exe"
  +------------------------------------------------------

* SEP: Insert a playlist separator before the current selection.

* DEL: Remove the current selection from the playlist.

* CROP: Remove *all but* the current selection.

* WIPE: Clear out all orphan entries, i.e., those whose file is missing (as
  indicated in red).
  With Shift: Remove duplicates.

* UNDO: Undo the last edit action.
  With Shift: Redo last Undo action.

* SAVE: Save playlist.

* LOAD: Load new playlist.

Note: These buttons apply to the Playlist only when it has focus (indicated
by a slight background illumination at the top and bottom). If the Library
is open and has focus, the buttons affect the focussed view there.

Other useful playlist actions:

* Cut/Copy/Paste key short-cuts: Copy entries between playlist and clipboard.
  This allows both exporting and importing playlist fragments as text or
  transferring between playlists (e.g., in the Library).

* More shortcuts for manipulating the selection can be found in the context
  menu (Right-click on playlist).

* Eject: To clear the playlist, use the Eject button on the Control pane.

* Shuffle: To activate random play, use the Shuffle button on the Control pane.
  Shuffle makes sure that every song is only played once. Fwd/bwd control
  buttons will go through the playlist in shuffle order. When adding new
  tracks to the playlist while Shuffle is active, no matter at which position,
  they will be inserted in the random order after any track whose position has
  already been observed. On the other hand, reordering entries in the
  "physical" playlist does not affect the order of random play.

* Reverse (context menu): Flip the order of the playlist or selection.

* Repair (context menu): Try to rediscover missing files (more in Section 3.6).

* Export (context menu): Copy all files in playlist or selection to a disk
  folder of choice. Export "with Position" renames them by prepending their
  relative position to the file name, so that they sort in play order.

* Search (context menu on an entry): Search current Library view for a track
  with the same artist and title (only available when Library is open).


[1] https://www.mp3tag.de


3.4 Playing Tracks Like a Pro: the Library
------------------------------------------

The Library is for power users who have a large and hopefully well-organised
music collection that they want to search and view from every angle. And
who perhaps want to crank out a good library of playlists, too.

The Library pane consists of two main areas:

* Browser (left): This is a browser for all music folders that have been added
  to the library.

* Views (right): This is a list of the tracks in a particular folder or
  sub-folder. It can be further divided into artist, album, and track view.

The library is organised around hierarchical folders. You add folders from disk
and Camp indexes them for its database.

* To add a folder to the database, either drag it onto the browser, or select
  it manually the old-fashioned way using the ADD button at the bottom. A
  folder added this way is called a *root*. You can add as many roots as you
  want, as long as they don't overlap, i.e., none is a super- or sub-folder of
  another on your drive.

* Once added, Camp will happily scan the new root and all its sub-folders for
  audio files and playlists. A scan in progress is indicated by the big yellow
  indicator light at the upper left corner of the Library pane.

* Camp does a quick rescan when started next time, to detect changes. You can
  also force that manually by clicking the scanning indicator.

When done, you can use the Browser to explore the folder structure and the
Views for the tracks in it.


3.4.1 The Browser
.................

The Browser is for browsing through the folders you have added: the roots,
their sub-folders, and also playlists in these folders.

In Camp, playlists are treated like virtual folders, meaning that they can
participate in all the interesting activities around searching, filtering,
sorting, showing meta data, etc.

There is an entry "All" at the top of the folder list. This is the virtual
parent folder of all roots.

Browser entries can be manipulated with a few buttons at the bottom:

* ADD: Add a new root to the browser.

* DEL: Remove a root from the browser. This button can also be used to remove
  a playlist, which will physically remove it from disk. This action is only
  executed when the playlist is empty! Regular sub-folders cannot be deleted.

* NEW: Create a new empty playlist file in the currrently selected folder.

* VIEW: Create a viewlist file reflecting the current View (see Section 3.5).

* SCAN: Rescan the selected folder (if the browser has focus), or the current
  selection in the View that currently has the focus.
  With Shift: Do a "thorough" rescan. A quick scan only looks for new or
  deleted files, while a throrough scan rereads all meta data.

Other things to do in the Browser:

* Reorder: You can change the order in which sub-folders are shown by dragging
  individual entries. You cannot move them to another folder, though.

* Rename (context menu): You can also change the name shown for an entry. Don't
  worry, this does not affect the physical name of the folder or file on disk.

* Repair (context menu): Go through all playlists under the selected folder and
  try to fix missing files (Section 3.6).

* Turn Playlists (context menu): Go through all playlists under the selected
  folder and modify how they store their file paths, either absolutely, or
  relatively to the location of the playlist.

  Absolute paths have the advantage that you can move the playlist around your
  drives and it will still find the tracks. Relative paths have the advantage
  that you can move the library as whole (audio files + playlists) and the
  playlists continue to function.

  By default, Camp produces absolute playlists. If you moved stuff around and
  broke your playlists, you can also use the Repair feature to recover from the
  tragedy (Section 3.6).


3.4.2 The Views
...............

The contents of a folder selected in the browser (including virtual folders
like playlists) can be viewed in a number of exciting ways.

* Tables: The view area can be hierarchically organised into artists, albums,
  and tracks tables, each of which can be toggled on or off using the
  respective buttons above the Browser.

* Grids: If you prefer a wall of covers, albums and tracks can alternatively be
  viewed as a cover grid. These options are cycled through by clicking the
  aforementioned view toggles multiple times.

* Select: Click on an entry to select it. Ctrl/Command-click toggles the
  selection of an individual track. Shift-click toggles the selection
  of the entire range between the currently and the last clicked entry.

* Queue: a double-click on a selected table or grid entry queues up all its
  tracks in the playlist; a triple-click clears out the previous playlist first.

  Tip: If you have selected multiple entries, then use double/triple click with
  Ctrl/Command to avoid changing the selection.

* Filter: Selecting one or multiple entries in the artists or albums table
  filters the downstream albums or tracks table to only show what belongs to the
  selected artists or albums or both, respectively.

* Columns: Columns in a table can be resized and reordered by drag & drop on
  the column header as you would probably expect. Right-click on a header
  yields a context menu to remove or add even more columns.

* Sort: Each table can be sorted according to up to 4 keys, by clicking column
  headers:

  - simple click to select a columns as primary sort key,
  - Shift-click for secondary,
  - Alt/Option-click for tertiary, and
  - Shift+Alt/Option-click for quaternary key.

  Clicking twice toggles the respective sort direction. Ctrl/Command-click
  disables a sort key.

* Search: Right-clicking on a table cell opens the context menu with an option
  to search the current folder for other entries with the same text as in that
  cell. Good way to e.g. find other songs by an artist.

* Cover: Clicking and holding on a cover thumbnail in table view opens a pop-up
  for the cover. Hold the mouse butten and hover over other entries to flick
  through covers on the fly.

* Save: the context menu also allows saving the current view as a playlist or
  as a viewlist (Section 3.5).

* Tag: Invoke an external tag editor (see Section 3.3 for how to fiddle with
  configuring that).

* Sort (context menu): In addition, the context menu of the tracks table when
  showing a playlist folder has an option to reorder that playlist to the
  current table sort order.

Additional options are available only for the tracks table, and only if it
displays a playlist folder, and only if that is currently sorted by position:

* Editing: Essentially, all edit functionality as for the main Playlist
  (Section 3.3) is available, including drag & drop, adding, removing,
  reordering, wiping, deduping, repairing, exporting tracks.

* Buttons: In particular, when the current focus is on the tracks table of a
  library playlist, then the playlist edit buttons at the bottom of the
  Playlist pane are rewired to act on the tracks table.

If you don't want to tediously customise the view for each and every folder
individually, then you can set the current folder's view as a default for other
folders (via Browser context menu). There are separate defaults for
playlist/viewlist folders and regular folders.


3.4.3 Searching
...............

In addition to filtering by selection (see Section 3.4.2), the contents of
the View area can be further constrained by spilling something into the Search
field. And this works even with playlists.

* Text Search: Typing one or multiple words filters the View to tracks
  whose meta data contains all the words. Example:

  +-----------------------
  | ed rush optical
  +-----------------------

  That is, search for tracks done by both Ed Rush & Optical.

* Clear: Clicking on SEARCH clears the search field.

* History: Use the Up/Down arrow keys to recycle your history of search terms,
  (like you're perhaps used to from shells like bash).

* Connectives: Separating words by `|` filters for tracks that have at least
  one of the sides. Example:

  +------------------------
  | ed rush|optical
  +------------------------

  That is, search for tracks done by either Ed Rush or Optical or both.
  Alternatively, perhaps ask for tracks by Optical that Rush was NOT involved
  in:
 
  +------------------------
  | ~rush optical
  +------------------------

  You can also use `&` if you like, but it's equivalent to just space.

* Grouping: Use parentheses to group search formulas:

  +------------------------
  | ~(rush|optical) virus
  +------------------------

  That is, search for tracks theat involved neither Rush nor Optical but were
  on Virus Recordings.

* Quoting: Throw in quotes for search terms that are not simple words:

  +------------------------
  | "ed rush"
  +------------------------

  That is, search for "Ed Rush", not just something that happens to contain
  both words. For example, this will not accidentally pick up a track named
  "Crushed".

* Attributes: The notation `#attr` produces the value of a specific meta data
  attribute of each track considered. It can be one of:

  - #title - track title
  - #artist - track artist
  - #album - album name
  - #albumartist - album's artist
  - #year - release year
  - #date - release date (expects tag in ISO-compliant YYYY-MM-DD format!)
  - #label - record label
  - #country - artist or release country
  - #track - track number
  - #disc - disc number
  - #disctrack - combined disc & track ("2.03" format, omitting disc if unset)
  - #tracks - total nubmer of tracks on disc
  - #discs - total number of discs in box
  - #length - palying time
  - #rating - rating value (on 1-5 scale)
  - #codec - Codec name (e.g. "FLAC" or "MP3")
  - #channels - number of channels
  - #depth - sample bit depth of (e.g. 16 for CD)
  - #samplerate - sampling rate (e.g. 44100 for CD)
  - #bitrate - bit rate (e.g. 128, 192, 320 or something for MP3s)
  - #pos - position in playlist (only meaningful for playlist entries)
  - #fileexists - true for track files that are not missing
  - #filetime - the modification time of a track's file
  - #filesize - the size of a track's file (in bytes)
  - #filename - the name of a track's file
  - #fileext - the extension of a track's file
  - #filedir - the path of the folder a track's file resides in
  - #filepath - the full path of a track's file
  - #now - the current date & time
  - #random - a random number (different every time)
  - #true - the true truth value
  - #false - the false truth value

  For example, only give me Ed Rush tracks that have not vanished from disk:

  +------------------------
  | "ed rush" #fileexists
  +------------------------

* Formulas: You can express arbitrary conditions with the following operators:

  - `&`, `|`, `~` - conjunction, disjunction, negation
  - `=`, `<>`, `<`, `>`, `<=`, `>=` - comparison
  - `@`, `~@` - text contained in, not contained in
  - `+`, `-`, `*` - addition, subtraction, multiplication
  - `++` - text concatenation

  For example, give me all tracks on the Virus label that are not ancient:

  +----------------------------------------
  | virus @ #label & #date >= 2000-06-30
  +----------------------------------------

* Constants: Formulas can involve the following types of literals:

  - text: either quoted or unquoted; quotes can be omitted when there are no
    spaces or funny characters in the text snippet
  - numbers: like 0, 1 or 100; suffixes like in 30M or 5Ki are also understood
    (K, M, G, T, P, E or Ki, Mi, Gi, Ti, Pi, Ei, to be precise)
  - time: written as consecutive numbers with suffixes like 1h30m10s or 2y20d
  - dates: in ISO format like 2000-06-30 or 2000/06/30
  - Booleans: use #true and #false if ever needed

* Type Checking: Formulas are actually checked for consistency, based on the
  5 data types for text, numbers, time values, date values, or Booleans.
  Mixing these types in a meaningless way is rejected, though most types can be
  implicitly converted to text.


3.5 Viewlists
-------------

In addition to plain playlists, which use the usual .m3u format, Camp also
understands "smart" playlists, called *viewlists*.

* File extension: A viewlist is stored as a file with the extension .m3v

* File format: It consists of a search formula like described in Section 3.4.3.
  This is applied to the entire music library (the virtual "All" folder) to
  generate a live playlist. (To restrict it to certain roots, use a formula
  constraining #filepath or #filedir.)

* Sorting: The search formula in a viewlist may be followed by `^` and a
  sequence of sort keys that define how the viewlist ought to be sorted. A sort
  key is one of the search attributes (Section 3.4.3), possibly preceeded by
  `-` to indicate reverse (descending) order. For example:

  +----------------------------------------
  | virus @ #label ^ #artist -#date
  +----------------------------------------

  That is, generate a playlist from all releases on the Virus label, sorted by
  artist, and secondarily by descending release date.

* Auto-generate: If you are viewing some folder with some search or
  artist/album filtering, then the VIEW button (or the context menu on the
  view) can create a new viewlist saving that view for eternity.


3.6 Repairing Playlists
-----------------------

The context menu on playlist tracks has an item for "repairing" a playlist. It
goes through all the tracks and checks if their files still exist. Where not, it
searches the library for a file with the same or a similar name.

* Once the search is complete, it shows a log of all missing files and whether
  they have been found somewhere else in the library. If you select OK, it will
  modify all missing entries for which substitutes were found.

* No regrets: The log is just a preview, if you Cancel nothing will be modified.

* The Repair command is also available in the browser. When requested on a real
  folder (as opposed to a single playlist), it applies to all playlists in that
  folder and its sub-folders.


3.7 UI Configuration
--------------------

There are various ways to tweak the user interface. All settings are remembered
across sessions:

* Color: Click the audio properties line (or the Control pane's context menu)
  to cycle through color schemes.

* Visualisation: Ctrl/Command-U (or the Control pane's context menu) cycles
  through different track vissualisations in the Control pane: Cover art, Wave,
  Oscilloscope, or All off.

* Resize: Drag on any window border to resize it. However, vertical resize is
  only possible when the Playlist pane is open, horizontal only when the
  Library pane is open.

* Dividers: Drag the dividers between Browser and Views, or between the
  individual view tables. The configuration of view dividers will be remembered
  per folder!

* Text Size: Use the + and - keys with Ctrl/Command (or the context menu from
  the Control pane) to adjust the size of all text in table, list, and edit
  widgets.

* Text Padding: Use the context menu from the Control pane to also adjust the
  vertical padding around all such text.

* Grid Size: Use the + and - keys with Shift+Ctrl/Command (or the context
  menu from the Control pane) to adjust the size of covers in grid views.

* Pop-up Size: Use the [ and ] keys with Ctrl/Command (or the context menu from
  the Control pane) to adjust the size of cover popups. But note that pop-ups
  cannot protrude from the main window, due to limitations of the graphics
  library. Hence their size will be limited by the current window size.

* Scaling: Use the [ and ] keys with Shift+Ctrl/Command to change the global
  scaling factor for the entire UI. This defaults to 1.

* Text Rendering: Shift+Ctrl/Command+U switches the rendering procedure for
  text. On low-resolution screens (up to 1440p) Camp defaults to plain
  rasterisation, while for higher resolution screens, it defaults to SDF [1].
  The defaults usually look better on the respective screens, but this option
  allows to switch manually.

  (That said, neither option looks particularly great with small text at a
  scaling factor of 1. That is a limitation of the rendering method of the
  underlying graphics library, that cannot do sub-pixel rendering. You may
  want to experiment with Text Sizes to find the one that looks best on your
  system.)

* Hide Covers: Ctrl/Command+Y hides the cover in the Control pane, whereas
  Shift+Ctrl/Command+Y hides the the covers in Library list views. That would
  primarily be for performance's sake.

* FPS: Ctrl/Command+U toggles the display of an FPS counter in the Control
  pane. Note that this is soft-limited to 60.

The interface does not use any rounded corners anywhere. That's a feature.


[1] https://steamcdn-a.akamaihd.net/apps/valve/2007/SIGGRAPH2007_AlphaTestedMagnification.pdf


3.8 Limitations
---------------

Camp comes with a few limitations, most of which I can conveniently blame on
the underlying graphics and audio library (Raylib, Section 4.3).

* CPU Usage: Camp uses an extravagant "immediate mode" GUI, which is a bit more
  expensive. It isn't super-optimised either. On a dated computer, it may use
  more compute than a backround app deserves. (And for some reason, I see much
  higher CPU consumption on my Mac laptop than on my Windows desktop, about 40%
  vs 2%, while GPU time is about the same.)

* Localisation: The underlying library doesn't offer much in the way of
  detecting keyboard assignments, so I'm afraid it assumes an English/U.S.
  keyboard. Feel free to hack the source code if you're desperate for an
  override.

* Text looks great on hi-res screens (like Retina screens or 4K displays), or
  with UI scaling. Not so much on lower resolution screens with a scaling of 1.
  As mentioned in Section 3.7, you may want to play with different text sizes
  to find the one that sucks least.

* The graphics library does not support multiple windows. Hence, the one window
  has to be a rectangular one and with that, the Library pane cannot be
  vertically resized when the Playlist isn't open. Likewise, pop-ups cannot be
  protrude from the parent window. Bummer.

* Window resizing or opening/closing Playlist or Library panes may create ugly
  animation artefacts for a moment, depending on operating system.

* More exotic audio formats are not supported. FLAC, MP3, WAV, and OGG are
  fine. In exchange, MOD and XM modules can be played out of the box!

* There may be bugs. Correction: there almost certainly are. And other
  occasional signs of immature software.


 _____________________________________________________________________________
|  4. THE SOURCE                                                              |
|_____________________________________________________________________________|

Don't look at it! It's ugly.

Seriously.

I mean it, it needs cleaning up.

Okay, if you insist, here are a few comments regarding the code and its
organisation.


4.1 Modules
-----------

* Everything in `lib_audio_file` is a generic library for retrieving meta data
  from audio files in various formats.

* `Data`, `Track`, and `Query` implement the internal data base representation.

* `Control`, `Playlist`, `Library`, `Filesel`, `Menu`, and `Log` encapsulate
  various states and behaviours of the respective components, independent of UI.

* `Table` and `Edit` are abstractions for the state of tables and edit fields.

* `Main` and the various `Run_*` modules implement the application-specific UI
  interaction of the different panes.

* `Layout` declares all the widgets used by the actual UI.

* `Ui` is a library of generic widgets implemented on top of the low-level API.

* `Api` abstracts away everything about the low-level graphics and audio layer,
  so that it could potentially be swapped out for something else.

* `State` and `Config` deal with global state and configuration persistence.

* `Storage` is a generic module for accessing application storage.

* `Bin` and `Text` are generic modules for persisting data in binary or text
  format.


4.2 Database
------------

I tried SQLite. It was clumsy and slow. Its interface is very stateful, the
global write lock a serious bottleneck, and all the copying between different
string representations doesn't help. Naively encoding large directory trees
into a single huge table is inefficient, but complicated to avoid. At the same
time, most queries are full search, so cannot be optimised much by the SQL
engine. In such a scenario, SQL actually provides litte benefit.

In the end a direct tree structure as an in-memory database turned out to be a
much better fit. It is simple, quick to traverse and moreover, individual nodes
can be updated without any lock, e.g., during a background rescan of the
library files.

The only nuisance is that persistance has to be implemented manually. Thus the
`Bin` module. Fortunately, that's fast enough to savely blast out the whole
database to disk frequently in a background thread.


4.3 Graphics and Audio
----------------------

The low-level graphics layer is implemented with the Raylib library [1] and its
excellent OCaml binding [2].

But everything is abstracted away in the `Api` module, which also works around
a number of quirks in that library. For example, its buggy mouse position
handling and its quirky window resizing/positioning behaviour. The module
also handles global graphics scaling.

Audio is particularly simple, since Raylib already does most of the work for
loading and buffering audio files. One annoyance is, however, that it does
not deal with UTF-8 file paths properly, so we have to copy every file with
a non-ASCII path to a temp directory before loading it.


[1] https://www.raylib.com
[2] https://github.com/tjammer/raylib-ocaml


4.4 User Interface
------------------

The actual UI takes an immediate mode approach, which has been all the hype
lately.

A set of generic widgets is implemented in `Ui` from first principles on top of
the low-level API. This code and its interface could really use some clean-up...

The `Layout` module then defines all widgets of the actual UI, relative to a
set of dynamic layout parameters.

The `Run_*` modules invoke the widget functions defined by `Layout` and
process their inputs. With immediate mode, we need to be extra careful to do
things in the right order, since the approach does not compose well. Sometimes,
the only easy way to achieve the correct drawing order is to use a delay
function that defers some drawing actions to the end of the frame. Hacky-hack.

Unfortunately, immediate mode also is very costly: naively, everything would be
redrawn on every frame, 60 times a second. On my desktop machine, that would
immediately spin up the fan and eat up up to 50% GPU time once I open the
Library pane with a lot of small text in it. To work around that issue, all
table widgets' graphics are cached into their private texture buffer per table
and only redrawn occasionally or when their content is dirty.

Another pain point is the logic for mouse drags. The `Ui` module essentially
implements some global state to track which widgets owns the mouse and what
the widget-specific state deltas are. There might be smarter ways of doing this.

In retrospect, this dumb man's conclusion is that, contrary to what you hear in
the news, immediate mode does not scale well to complex UIs. Once the necessary
caching logic, state threading, and ordering hacks are bolted on, many of its
alleged advantages are lost on the floor, and you are left with a brittle and
leaky abstraction. If I had time, I would like to implement a more robust, more
declarative layer on top.


 _____________________________________________________________________________
|  5. LEGAL STUFF                                                             |
|_____________________________________________________________________________|

I put Camp and its source code under an attribution non-commercial share-alike
Creative Commons license, CC BY-NC-SA 4.0 [1] for short. Enjoy!


[1] https://creativecommons.org/licenses/by-nc-sa/4.0/
