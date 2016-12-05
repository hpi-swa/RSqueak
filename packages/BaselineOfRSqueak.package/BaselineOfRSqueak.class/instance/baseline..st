baseline
baseline: spec
	<baseline>
	spec
		for: #'common'
		do: [
			spec
				configuration: 'FFI'
					with: [ spec repository: 'http://www.squeaksource.com/MetacelloRepository' ];
				package: 'SharedPool-Speech-Klatt'
					with: [ spec
						file: 'SharedPool-Speech-dtl.2';
						repository: 'http://www.squeaksource.com/Speech' ];
				package: 'VMMakerOSCog'
					with: [ spec
						preLoadDoIt: #updateImage;
						requires: #('FFI' 'SharedPool-Speech-Klatt');
						file: 'VMMaker.oscog-nice.1812';
						repository: 'http://source.squeak.org/VMMaker' ];
				package: 'RSqueak'
					with: [ spec
						repository: 'github://HPI-SWA-Lab/RSqueak:master/repository';
						postLoadDoIt: #setRSqueakPreferences ];
				package: 'TopazRuby'
					with: [ spec
						repository: 'github://HPI-SWA-Lab/RSqueak:master/repository' ];
				yourself.
			spec
				group: 'default' with: #('VMMakerOSCog' 'RSqueak' 'TopazRuby') ];
				yourself
