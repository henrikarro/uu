package com.reallifedeveloper.uu_1dl251.domain;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

import com.reallifedeveloper.uu_1dl251.domain.Role;
import com.reallifedeveloper.uu_1dl251.domain.User;

public class UserTest {

	private static final Role ROLE1 = new Role("ROLE1", "Test role 1");

	@Test
	public void hasRole() {
		User user = new User("foo", Arrays.asList(ROLE1));
		Assert.assertTrue("User foo should have role 1", user.hasRole("ROLE1"));
		Assert.assertFalse("User foo should not have role 2", user.hasRole("ROLE2"));
	}
}
