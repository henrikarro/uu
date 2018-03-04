package com.reallifedeveloper.uu_1dl251.domain;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.Table;

@Entity
@Table(name = "user")
public class User {

	@Id
	@Column(name = "user_id")
	private String userId;

	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "user_role", joinColumns = {
			@JoinColumn(name = "user_id", referencedColumnName = "user_id") }, inverseJoinColumns = {
					@JoinColumn(name = "role_id", referencedColumnName = "role_id") })
	private Set<Role> roles = new HashSet<>();

	public User(String userId, List<Role> roles) {
		if (userId == null || roles == null) {
			throw new IllegalArgumentException("Arguments must not be null: userId=" + userId + ", roles=" + roles);
		}
		this.userId = userId;
		this.roles = new HashSet<>(roles);
	}

	// Required by Hibernate
	User() {
	}

	public String userId() {
		return userId;
	}

	public Set<Role> roles() {
		return roles;
	}

	public boolean hasRole(String roleId) {
		return roles.stream().anyMatch(r -> r.roleId().equals(roleId));
	}

	@Override
	public String toString() {
		return "User{userId=" + userId + ", roles=" + roles + "}";
	}

}
