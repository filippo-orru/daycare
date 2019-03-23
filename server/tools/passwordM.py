from passlib.hash import argon2

encrypt = argon2.hash

verify = argon2.verify