import secrets

def generate():
    return secrets.token_urlsafe(32)

if __name__ == "__main__":
    print(generate())