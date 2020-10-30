terraform {
  required_providers {
    mikrotik = {
      source  = "ddelnano/mikrotik"
      version = "~> 0.3"
    }
  }
}

provider "mikrotik" { }

resource "mikrotik_dhcp_lease" "moto-g7-1" {
  address = "192.168.88.60"
  macaddress = "D0:04:01:07:C8:BA"
  comment = "moto g7 phone"
}
