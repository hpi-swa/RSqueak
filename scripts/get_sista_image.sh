JOB_NAME=Scorch
REPO=http://smalltalkhub.com/mc/ClementBera/Scorch/main

rm -rf pharo-vm
rm -rf pharo-local
rm -rf Scorch*.image
rm -rf Scorch*.changes
rm -rf Pharo*.image
rm -rf Pharo*.changes

#get latest Pharo 6 image
wget --quiet -O - get.pharo.org/60+vm | bash

cp pharo-vm/PharoV50.sources .

#rename to the specified name
mv Pharo.image $JOB_NAME.image
mv Pharo.changes $JOB_NAME.changes

#Image preparation: disable freetype for non freetype compatible VMs
sista32/squeak $JOB_NAME.image eval \#\(FT2Handle FreeTypeSettings FreeTypeCache\) do: \[ :e \| \
 SessionManager default unregisterClassNamed: e \]. \
 Smalltalk snapshot: true andQuit: true

#Image preparation: put correct value in special object array, patch mourner bug.
sista32/squeak $JOB_NAME.image eval Smalltalk specialObjectsArray at: 60 put: \#trapTripped .\
 Author fullName: \'benchmarker\'.\
 Smalltalk specialObjectsArray at: 59 put: nil .\
 WeakArray class compile: \'primitiveFetchMourner \< primitive: 172 error: ec \> ^ nil\' classified: \#patch .\
 Smalltalk snapshot: true andQuit: true

sista32/squeak $JOB_NAME.image eval \[ ScriptLoader \
 loadLatestPackage: \'Benchmark\' \
 from: \'http://www.hpi.uni-potsdam.de/hirschfeld/squeaksource/BenchmarkRunner\'\; \
 loadLatestPackage: \'SMark\' \
 from: \'http://www.hpi.uni-potsdam.de/hirschfeld/squeaksource/BenchmarkRunner\'. \] on: Warning do: \[ :ex \| ex resume \].\
 Smalltalk snapshot: true andQuit: true

#Load scorch and run basic tests
sista32/squeak $JOB_NAME.image config $REPO ConfigurationOfScorch --install=bleedingEdge
# sista32/squeak $JOB_NAME.image test --junit-xml-output "ScorchingTests.*"

#Recompilation with FullBlock and new bytecode set. Opens the Transcript and run the default validations.
sista32/squeak $JOB_NAME.image eval CompilationContext bytecodeBackend: OpalEncoderForSistaV1 .\
 CompilationContext usesFullBlockClosure: true .\
 OpalCompiler recompileAll.\
 Transcript clear.\
 Transcript open.\
 SoValidator new validateMethods: SoValidator allExamples passes: \#\(O0 O1 \#O1b O2\) options: \#\( graphSanity reportToTranscript \).\
 Smalltalk snapshot: true andQuit: true

