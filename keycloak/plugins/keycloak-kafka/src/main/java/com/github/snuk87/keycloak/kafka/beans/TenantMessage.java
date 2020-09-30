package com.github.snuk87.keycloak.kafka.beans;

public class TenantMessage {

	private String type;
	private String tenant;

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getTenant() {
		return tenant;
	}

	public void setTenant(String tenant) {
		this.tenant = tenant;
	}

}
