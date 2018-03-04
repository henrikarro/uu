package com.reallifedeveloper.uu_1dl251.fitnesse;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import com.reallifedeveloper.tools.test.fitnesse.AbstractFitNesseFixture;
import com.reallifedeveloper.uu_1dl251.domain.Role;
import com.reallifedeveloper.uu_1dl251.domain.RoleRepository;
import com.reallifedeveloper.uu_1dl251.domain.User;
import com.reallifedeveloper.uu_1dl251.domain.UserRepository;

public class AddUser extends AbstractFitNesseFixture {

	private String userId;
	private List<String> roleNames;

	private UserRepository userRepository;
	private RoleRepository roleRepository;

	public AddUser() {
		super("META-INF/spring-context-uu-1dl251-func-test.xml");
		this.userRepository = getBean(UserRepository.class);
		this.roleRepository = getBean(RoleRepository.class);
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public void setRoleNames(String commaSeparatedRoles) {
		this.roleNames = Arrays.asList(commaSeparatedRoles.split(","));
	}

	public void execute() {
		List<Role> roles = roleNames.stream().map(s -> roleRepository.findByRoleId(s)).collect(Collectors.toList());
		logger().debug("roles=" + roles);
		User user = new User(userId, roles);
		userRepository.save(user);
	}
}
