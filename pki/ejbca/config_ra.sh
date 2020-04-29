#!/bin/bash

password=${EJBCA_PASS:-secret}

#First we create a new end entity for soap_client
/opt/primekey/bin/ejbca.sh ra addendentity --username soap_client --dn  \
"CN=soap_client" --password ${password} --caname IOTmidCA \
--type 1 --token P12

#If the soap_client already exists (persisted) we renew his status
/opt/primekey/bin/ejbca.sh ra setendentitystatus soap_client -S 10

#To batch a .p12 file, we need to reset the soap client pwd
/opt/primekey/bin/ejbca.sh ra setclearpwd soap_client ${password}

# we batch the .p12 certficate
/opt/primekey/bin/ejbca.sh batch

# Give the new RA adm previleges (the RA must be an administrator)
/opt/primekey/bin/ejbca.sh roles addrolemember "Super Administrator Role" \
IOTmidCA WITH_COMMONNAME TYPE_EQUALCASE soap_client

# get the ca certificate
/opt/primekey/bin/ejbca.sh ca getcacert --caname IOTmidCA -f /opt/p12/ca.crt

# delete the two first line
for i in 1 2; do
    sed -i '1d' /opt/p12/ca.crt
done

#import ca profiles and user profiles
/opt/primekey/bin/ejbca.sh ca importprofiles -d "/opt/app/profiles"

