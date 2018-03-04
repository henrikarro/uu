package com.reallifedeveloper.uu_1dl251.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "role")
public class Role {

	@Id
	@Column(name = "role_id")
	private String roleId;

	private String description;

	public Role(String roleId, String description) {
		if (roleId == null || description == null) {
			throw new IllegalArgumentException(
					"Arguments must not be null: roleId=" + roleId + ", description=" + description);
		}
		this.roleId = roleId;
		this.description = description;
	}

	// Required by Hibernate
	Role() {
	}

	public String roleId() {
		return roleId;
	}

	public String description() {
		return description;
	}

	@Override
	public String toString() {
		return "Role{roleId=" + roleId + ", description=" + description + "}";
	}

}
