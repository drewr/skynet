class common::users {
  user { "aar":
    password => "********",
    comment => "Drew Raines",
    shell => "/usr/bin/zsh",
    require => [Package["zsh"]],
  }
}
