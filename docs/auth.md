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

# Password

1. Reset passwrod, when user want to generate new one and logged in.
   Password reset is allowed only once per month.
   `/auth/password/new` - The user need to be logged in to perform this opertion.
   Response: Unit, Error: you already took advantage of reseting password. Next attempt will be avaialbsle in (n days)
2. Forgot password
   `/auth/password/reset` - Attempts to reset is limited by 3 to prevent malicious users of floodiing email
   Request: email, Error: email not found, attempts exceeded
3. Regenerate new password
   `/auth/password/regenerate`
   Request: token, captcha, password, once again password, Error: token not valid, token expired, captcha error, password weak, passwords mismatched
4. Check token before rendering reseet form.
   `/auth/password/token/check`
   Request: token, captcha, Response: bool, Error: captcha error
