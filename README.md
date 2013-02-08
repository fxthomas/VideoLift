# VideoLift

**VideoLift** is a small automatic TV shows and movie library, part of a Scala project at Télécom ParisTech.

It fetches metadata automatically from file names : e.g.
`Awesome.Show.s01e02.hdtv.XVid.avi` becomes `Awesome Show, Season 1, Episode
2`, and gets descriptions, episode names, cover images and more!

![Screenshot](https://raw.github.com/fxthomas/VideoLift/master/images/Screenshot.png)

# How to build?

You need to :

  * Get Scala and SBT (Simple Build Tool)
  * Edit the `src/main/resources/props/default.props` and add your DB settings and file library paths
