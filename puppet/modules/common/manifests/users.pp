class common::users {
  $user = "aar"
  $home = "/home/aar"

  user { $user:
    ensure => "present",
    purge_ssh_keys => true,
    home => $home,
    password => "********",
    comment => "Drew Raines",
    shell => "/usr/bin/zsh",
    require => [Package["zsh"]],
  }

  file { $home:
    ensure => directory,
    owner => $user,
    require => User[$user],
  }

  ssh_authorized_key { $user:
    user => $user,
    type => 'ssh-rsa',
    key  => 'AAAAB3NzaC1yc2EAAAABIwAAAMUAsgWskmtNVP1cFedYFPhNemQUgHJZRhsy/Yg9AY7Abj3mHkum1o45QOYzT7raBJ2teBx0Sy+dMEMjX/q/1i7rvkzIO3BzywwXFUWO1Vhxh7texp1yD0WXGPy97BxmCgQFCZBZ2FDI1jrWa6UCmQWhC0eo014AuIDO9Fb7m9zBVLh0lKORt34mkzsAaYZjo4o0CpvbElW6P/TxhxBTlbl76oC24apy8XuEX9yqBfTVLm+JAn6eNMSzvJUoCDDh9pxP+faooQ==',
  }

}
