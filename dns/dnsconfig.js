var REG_NONE = NewRegistrar('none', 'NONE')
var DNS_GCLOUD = NewDnsProvider("gcloud", "GCLOUD");
var DNS_BIND = NewDnsProvider('bind', 'BIND');  // ISC BIND.

D("example.com", REG_NONE, DnsProvider(DNS_BIND),
    A("test","1.2.3.4")
);

D("draines.com", REG_NONE, DnsProvider(DNS_GCLOUD),
  A("test","1.2.3.5"),
  AAAA("test","dead:beef::0")
);
