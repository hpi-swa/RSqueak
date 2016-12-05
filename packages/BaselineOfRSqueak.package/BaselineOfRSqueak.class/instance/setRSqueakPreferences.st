baseline
setRSqueakPreferences
  "We clean up a little and so we get a good initial experience"
  ReleaseBuilder classPool at: 'DeferredTask' put: nil.
  TheWorldMainDockingBar instance closeAllWindows.
  World setAsBackground: (Color r: 0.896 g: 0.896 b: 1).
  Preferences toggle: #fastDragWindowForMorphic.
  TheWorldMainDockingBar instance toggleColorfulWindows.
  (HelpBrowser openOn: RSqueakHelp) model showFirstTopic.