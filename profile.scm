(parameterize ((compile-profile 'source))
  (load "main.scm"))

(profile-dump-html)
