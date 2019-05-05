# OCamlAlert

OCamlAlert is a TCP based alert system. It’s an alert system like CUNY’s alert system. CUNY’s alert system works in such a way that all registered users are sent a text message informing them about emergencies like the school being closed down due to severe weather of if there is an active shooter and even fires. In the case of OCamlAlert, any user that is connected to the server will receive an alert that an administrator sends out. It uses Lwt, plain TCP sockets from the Unix library and consists of the following three parts:

	1. The Server

	2. The User Client

	3. The Admin Client


## Building Binaries
	make all

## Starting the server:
	./server

## Starting the client
	./client

## Pushing out alerts
	./admin SuperSecretPassword SEVERE SNOWSTORM IN 15 MINS! STAY SAFE! STAY OFF THE ROADS!