instance creation
new: size withAll: value

	^ self immutableFrom: (Array new: size withAll: value)