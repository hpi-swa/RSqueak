baseline
updateImage
  Smalltalk at: #MCMcmUpdater ifPresent: [ :updater |
  [ (updater respondsTo: #doUpdate)
    ifTrue: [ updater doUpdate ]
    ifFalse: [ (updater respondsTo: #updateFromDefaultRepository)
                ifTrue: [ updater updateFromDefaultRepository ]] ]
    on: Warning do: [ :ex | ex resume: true ]].
