from passlib.hash import argon2


def encrypt(p):
    return argon2.hash(p)


def verify(p1, p2):
    return argon2.verify(p1, p2)


if __name__ == "__main__":
    print(
        verify(
            "123456",
            encrypt("123456")
            # "$argon2i$v=19$m=102400,t=2,p=8$GWPsndPa21sLgRACwHhvzQ$ts/uTXV2952Gigt/rGn+ZA"
        ))
