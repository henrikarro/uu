package com.reallifedeveloper.uu_1dl251.fitnesse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.reallifedeveloper.uu_1dl251.domain.Role;
import com.reallifedeveloper.uu_1dl251.domain.RoleRepository;
import com.reallifedeveloper.uu_1dl251.domain.User;
import com.reallifedeveloper.uu_1dl251.domain.UserRepository;
import com.reallifedeveloper.uu_1dl251.infrastructure.persistence.JpaRoleRepository;
import com.reallifedeveloper.uu_1dl251.infrastructure.persistence.JpaUserRepository;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { "classpath:META-INF/spring-context-uu-1dl251-func-test.xml" })
public class AddUserTest {

	@Autowired
	private UserRepository userRepository;

	@Autowired
	private RoleRepository roleRepository;

	@Before
	public void init() {
		((JpaUserRepository) userRepository).deleteAll();
		((JpaRoleRepository) roleRepository).deleteAll();
		((JpaRoleRepository) roleRepository).save(new Role("ROLE1", "Role 1"));
		((JpaRoleRepository) roleRepository).save(new Role("ROLE2", "Role 2"));
		((JpaRoleRepository) roleRepository).save(new Role("ROLE3", "Role 3"));
		((JpaUserRepository) userRepository).flush();
		((JpaRoleRepository) roleRepository).flush();
	}

	@Test
	public void execute() {
		final String userId = "foo";
		AddUser fixture = new AddUser();
		fixture.setUserId(userId);
		fixture.setRoleNames("ROLE1,ROLE3");
		fixture.execute();
		User user = userRepository.findByUserId(userId);
		Assert.assertNotNull("User foo should exist", user);
		Assert.assertEquals("User should have ID " + userId, userId, user.userId());
		Assert.assertTrue("User should have role ROLE1", user.hasRole("ROLE1"));
		Assert.assertFalse("User should not have role ROLE2", user.hasRole("ROLE2"));
		Assert.assertTrue("User should have role ROLE3", user.hasRole("ROLE3"));
	}
}
