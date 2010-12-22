# JO FRLY

<img src="http://boinkor.net/misc/jofrli.jpg">

A redis-backed url shortener in Common Lisp.

## Words of warning

* Authorization is not extremely well thought out. In particular,
  auto-generated API keys are an MD5 of a secret and a UUID. You may want to
  use your own api key generation routine if this bothers you (-:

* Also, there is no rate limiting. It's probably pretty easy to DDOS
  this system (and redis) out of existence. But who would want to do
  such a thing?

* (Perhaps worst of all) the hashing function for URLs to short
  strings isn't very well-tested, and might not distribute values as
  nicely as you might want. If you want a URL shortener that can't be
  walked, this may not be the right one for you.

## Getting started

1. Install redis
2. Add this directory to your `asdf:*central-registry*`
3. Load it (and dependencies) via quicklisp:

        (ql:quickload :jfrli)
        
4. Generate an API key: 

        (redis:with-connection () (jfrli:generate-authkey "some secret"))

5. (Optionally) set a minimum length for URL IDs:

        (setf jofrli:*initial-min-length* 2
              jofrli:*max-fill* 0.6)

   **Note**: these values get stored in Redis after the first access, so
    be sure to set them before you start jofrli for the first
    time. Altering them in the DB will affect how URLs are interned.
6. Start the Web service:

        (jofrli:start :api-port 6969 :redirect-port 6970)

7. Set up a proxy that redirects api.jofr.li to localhost:6969 and
   jofr.li to localhost:6970.

8. (Optionally) If you're running on another domain, adjust the base URI for 
   your redirection service:

        (setf jofrli:*base-url* (puri:parse-uri "http://whr.sm/"))