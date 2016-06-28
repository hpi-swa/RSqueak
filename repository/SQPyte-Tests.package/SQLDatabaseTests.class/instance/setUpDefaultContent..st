as yet unclassified
setUpDefaultContent: database
	database execute: 'INSERT INTO employees VALUES (?, ?, ?)' args: #(1. 'John Smith'. 12.5).
	database execute: 'INSERT INTO employees VALUES (?, ?, ?)' args: #(2. 'Jane Smith'. 16.7).