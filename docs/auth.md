# Roles and Permissions

roles:

                  root
          _________|_________
          |                  |
         user             provider
                             |
                           editor
                             |
                           guest
permissions:

                      root
              _________|_________
             |                   |
            user            providerAdmin
                          _______|_________
                          |               |
                    providerEditor   providerSettings
                          |
                    providerGuest

provider - full access to any api of secondary user

